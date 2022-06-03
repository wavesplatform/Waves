FROM openjdk:11-jre-slim

ARG WAVES_LOG_LEVEL
ARG WAVES_HEAP_SIZE
ARG WAVES_NETWORK
ARG ENABLE_GRPC
ARG PRIVATE_NODE

ENV WAVES_LOG_LEVEL=${WAVES_LOG_LEVEL:-INFO}
ENV WAVES_HEAP_SIZE=${WAVES_HEAP_SIZE:-2g}
ENV WAVES_NETWORK=${WAVES_NETWORK:-mainnet}
ENV ENABLE_GRPC=${ENABLE_GRPC:-true}
ENV PRIVATE_NODE=${PRIVATE_NODE:-false}

COPY build-scripts /tmp/

# Additional dependencies
RUN /bin/bash /tmp/setup-deps.sh

# Node files
ENV WVDATA=/var/lib/waves
ENV WVLOG=/var/log/waves
ENV WAVES_INSTALL_PATH=/usr/share/waves
COPY target /tmp/
COPY waves.custom.conf /tmp/waves.conf
RUN /bin/bash /tmp/setup-node.sh

EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins
WORKDIR $WVDATA

RUN /bin/bash /tmp/setup-private-node.sh

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
