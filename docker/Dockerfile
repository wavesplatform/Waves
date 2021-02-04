FROM debian:stable-slim
ARG WAVES_LOG_LEVEL="INFO"
ARG WAVES_HEAP_SIZE="2g"
ARG WAVES_NETWORK="mainnet"
ARG ENABLE_GRPC="true"

ENV WAVES_LOG_LEVEL=$WAVES_LOG_LEVEL
ENV WAVES_HEAP_SIZE=$WAVES_HEAP_SIZE
ENV WAVES_NETWORK=$WAVES_NETWORK
ENV ENABLE_GRPC=$ENABLE_GRPC

COPY build-scripts /tmp/

# Additional dependencies
RUN /bin/bash /tmp/setup-deps.sh

# Node DEB files
COPY target /tmp/
RUN /bin/bash /tmp/setup-node.sh
RUN rm -rf /tmp/*

USER waves
WORKDIR /var/lib/waves

EXPOSE 6869 6868 6863 6862 6870
VOLUME /var/lib/waves
VOLUME /usr/share/waves/lib/plugins

ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
