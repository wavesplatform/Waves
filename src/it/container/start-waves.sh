#!/bin/bash

echo Waves: $WAVES_NET_IP
echo Options: $WAVES_OPTS

java -Dwaves.network.declared-address=$WAVES_NET_IP:$WAVES_PORT \
     -Dlogback.pattern='[%-6(${waves.network.node-name})] %date{HH:mm:ss.SSS} %.-1level [%thread] %logger{26} - %msg%n' \
     -Dlogback.overrides=/opt/waves/logback-docker.xml \
     $WAVES_OPTS \
     -jar /opt/waves/waves.jar /opt/waves/template.conf
