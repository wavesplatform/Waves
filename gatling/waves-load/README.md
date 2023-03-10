# Gatling Template Project

Template project for Gatling performance tests

## Project structure

```
src.test.resources - project resources
src.test.scala.com.wavesplatform.wavesload.cases - simple cases
src.test.scala.com.wavesplatform.wavesload.scenarios - common load scenarios assembled from simple cases
src.test.scala.com.wavesplatform.wavesload - common test configs
```

## Test configuration

Pass this params to JVM using -DparamName="paramValue" AND -Dconfig.override_with_env_vars=true

```
Gatling logs:
CONSOLE_LOGGING=ON - turn on console logging
FILE_LOGGING=ON - turn on logging in file "target/gatling/gatling.log"
GRAYLOG_LOGGING=ON - turn on logging in Graylog
    graylog params:
        GRAYLOG_HOST - Graylog host
        GRAYLOG_PORT - on which port Graylog input is
        GRAYLOG_STREAM - the name of Graylog stream

Gatling metrics in InfluxDB:
GRAPHITE_HOST - influxdb with configured graphite plugin host
GRAPHITE_PORT - see /etc/influxdb/influxdb.conf: bind-address
INFLUX_PREFIX - see /etc/influxdb/influxdb.conf: database
```

Also, you can pass all params from Gatling-picatinny or use custom params
read: https://github.com/Tinkoff/gatling-picatinny/blob/master/README.md

## Debug

1. Debug test with 1 user, requires proxy on localhost:8888, eg using Fiddler or Wireshark

```
"Gatling/testOnly com.wavesplatform.wavesload.Debug"
```

2. Run test from IDEA with breakpoints

```
com.wavesplatform.GatlingRunner
```

## Launch test

```
"Gatling/testOnly com.wavesplatform.wavesload.MaxPerformance" - maximum performance test
"Gatling/testOnly com.wavesplatform.wavesload.Stability" - stability test
```

## Help

Telegram: @qa_load

Gatling docs: https://gatling.io/docs/gatling/reference/current/core/injection/
