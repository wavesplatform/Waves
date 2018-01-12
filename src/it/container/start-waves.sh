#!/bin/bash

echo Waves: $WAVES_NET_IP
echo Options: $WAVES_OPTS

java -Dwaves.network.declared-address=$WAVES_NET_IP:$WAVES_PORT $WAVES_OPTS -jar /opt/waves/waves.jar /opt/waves/template.conf
