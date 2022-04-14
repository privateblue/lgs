import scala.io.{Source, Codec}
import scala.util.matching.Regex

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.io.File
import java.nio.charset.CodingErrorAction

val separator = ";"

@main def lgs(dirname: String): Unit = {
  val dir = new File(dirname)
  if dir.exists && dir.isDirectory then
    dir.listFiles
      .filter(f => f.isFile && f.getName.startsWith("appserver."))
      .sortBy(f => f.getName)
      .iterator
      .flatMap(fileParser)
      .filter(l => l.requestLogLine.isDefined)
      .map(l => s"${l.timestamp}$separator${l.requestLogLine.get}")
      .foreach(println)
}

def fileParser(file: File): Iterator[LogLine] =
  Source.fromFile(file)(Codec.defaultCharsetCodec.onMalformedInput(CodingErrorAction.IGNORE)).getLines
    .filter(l => l.startsWith("DEBUG") || l.startsWith("INFO") || l.startsWith("ERROR") || l.startsWith("WARN"))
    .map(lineParser.run(_)._2)

def lineParser =
  for {
    level <- takeWhile(_ != ' ')
    timestamp <- takeWhile(_ != ' ').map { ts =>
      ZonedDateTime.parse(ts, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toInstant.toEpochMilli
    }
    _ <- extract(raw"(\[.*\]).*".r)
    from <- takeWhile(_ != ' ')
    rest <- State.get[String]
    requestLogLine = if from == "c.s.a.s.LoggingRequestEventListener" then
        Some(requestLogLineParser.run(rest)._2)
      else Option.empty[RequestLogLine]
  } yield LogLine(level, timestamp, from, rest, requestLogLine)

def requestLogLineParser =
  for {
    requestId <- takeWhile(_ != ' ').map {
      case rid if rid.startsWith("R^") =>
        R(rid)
      case rid if rid.startsWith("ScreenshotCapture^") =>
        val parts = rid.split("\\*")
        val screenshot = parts.find(_.startsWith("ScreenshotCapture^"))
        val invocation = parts.find(_.startsWith("Invocation^"))
        val r = parts.find(_.startsWith("R^"))
        ScreenshotCapture(screenshot.get, invocation, R(r.get))
      case rid => Other(rid)
    }
    user <- takeWhile(_ != ' ')
    startFinish <- takeWhile(_ != ' ')
    results <- if startFinish == "Finish" then
        for {
          ft <- takeWhile(_ != ' ').map(raw"\d+".r.findFirstIn(_).map(_.toLong))
          st <- takeWhile(_ != ' ').map(_.stripPrefix("httpStatus:"))
        } yield (ft, Some(st))
      else State.point((Option.empty[Long], Option.empty[String]))
    (finishTime, status) = results
    _ <- takeWhile(_ != ' ')
    url <- takeWhile(_ != ' ')
    metricsText <- State.get[String]
    metrics = parseMetrics(metricsText)
  } yield RequestLogLine(requestId, user, startFinish, finishTime, status, url, metrics)

def parseMetrics(metricsText: String): Map[String, Double] =
  metricsText.split(" ")
    .map(_.split("="))
    .collect {
      case l if l.size == 2 => (l.head, raw"\d+\.*\d*".r.findFirstIn(l.tail.head).getOrElse("0").toDouble)
    }
    .toMap

def takeWhile(p: Char => Boolean) =
  State[String, String](l => (l, l.takeWhile(p)))
    .flatMap(advance)

def extract(r: Regex) =
  State[String, String]  { l =>
    l match {
      case r(out) => (l, out)
      case _ => (l, "")
    }
  }.flatMap(advance)

def advance(v: String) =
  State[String, String](l => (l.drop(v.size).trim.dropWhile(_ == '-').trim, v))

case class LogLine(
  level: String,
  timestamp: Long,
  from: String,
  rest: String,
  requestLogLine: Option[RequestLogLine]
)

case class RequestLogLine(
  requestId: RequestId,
  user: String,
  startFinish: String,
  finishTime: Option[Long],
  status: Option[String],
  url: String,
  metrics: Map[String, Double]
) {
  override def toString =
    s"""$requestId$separator$user$separator$startFinish$separator${finishTime.map(_.toString).getOrElse("")}$separator${status.getOrElse("")}$separator$url"""
}

trait RequestId
case class R(value: String) extends RequestId {
  override def toString = s"$value$separator$separator"
}
case class ScreenshotCapture(id: String, invocation: Option[String], r: R) extends RequestId {
  override def toString = s"""$id$separator${invocation.getOrElse("")}$separator$r"""
}
case class Other(value: String) extends RequestId {
  override def toString = s"$value$separator$separator"
}

case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (ns, na) = run(s)
    (ns, f(na))
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (ns, na) = run(s)
    f(na).run(ns)
  }
}

object State {
  def point[S, A](v: A) = State[S, A](s => (s, v))
  def get[S] = State[S, S](s => (s, s))
}
