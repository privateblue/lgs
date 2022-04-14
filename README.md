# install Scala
Follow instructions here: https://docs.scala-lang.org/getting-started/index.html

# clone this repo
```git clone git@github.com:privateblue/lgs.git```

# compile the source
```scalac lgs.scala```

# run the script to create a csv
```scala lgs /path/to/logbundle/log/appserver > requests.csv```

# notes
Make sure that you use the same separator when importing the csv, as the one set in the code (line 9).

CSV columns:
* timestamp (long)
* timestamp (iso)
* requestId ScreenshotCapture
* requestId Invocation
* requestId R
* user
* Start / Finish
* finish duration (if Finish)
* response code (if Finish)
* url
* Datasource monitor
* Cache monitor
* Processing monitor
* GC monitor
* Request_Queue monitor
* Calc_Engine_Queue monitor
* Seeq_Database monitor
* Total monitor
* Datasource_Samples_Read monitor
* Datasource_Capsules_Read monitor
* Cache_Samples_Read monitor
* Cache_Capsules_Read monitor
* Cache_In-Memory_Samples_Read monitor
* Cache_In-Memory_Capsules_Read monitor
* Database_Items_Read monitor
* Database_Relationships_Read monitor
