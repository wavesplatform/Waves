#!/bin/bash
echo Options: $WAVES_OPTS
exec java $WAVES_OPTS -cp "/usr/share/waves/lib/*" com.wavesplatform.Application /opt/waves/template.conf
