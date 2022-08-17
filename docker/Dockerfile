FROM openjdk:11-jre-slim

ENV WAVES_LOG_LEVEL=INFO
ENV WAVES_HEAP_SIZE=2g
ENV WAVES_NETWORK=mainnet

COPY build-scripts /tmp/

# Additional dependencies
RUN /bin/bash /tmp/setup-deps.sh

# Node files
ENV WVDATA=/var/lib/waves
ENV WVLOG=/var/log/waves
ENV WAVES_INSTALL_PATH=/usr/share/waves
ENV WAVES_CONFIG=/etc/waves/waves.conf
COPY target /tmp/
COPY waves.conf.template $WAVES_CONFIG

ARG INCLUDE_GRPC=true
RUN /bin/bash /tmp/setup-node.sh $INCLUDE_GRPC

EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins
WORKDIR $WVDATA

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
