#!/bin/bash

WAVES_IP=`ifconfig eth1 | awk '/inet addr/ {gsub("addr:", "", $2); print $2}'`

java -Dwaves.network.declared-address=$WAVES_IP:$WAVES_PORT $WAVES_OPTS -jar /opt/waves/waves.jar /opt/waves/template.conf
