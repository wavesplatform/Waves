FROM openjdk:8-jdk-stretch as builder
ARG WAVES_VERSION="latest"
ARG DEB_PACKAGE_NETWORKS

ENV WAVES_VERSION=$WAVES_VERSION
ENV BRANCH=$BRANCH
ENV DEB_PACKAGE_NETWORKS=$DEB_PACKAGE_NETWORKS

COPY build.sh /tmp/
COPY target waves_current
RUN /bin/bash /tmp/build.sh

FROM debian:stable-slim
ARG WAVES_LOG_LEVEL="INFO"
ARG WAVES_HEAP_SIZE="2g"
ARG WAVES_NETWORK="mainnet"
ARG ENABLE_GRPC="false"

ENV WAVES_LOG_LEVEL=$WAVES_LOG_LEVEL
ENV WAVES_HEAP_SIZE=$WAVES_HEAP_SIZE
ENV WAVES_NETWORK=$WAVES_NETWORK
ENV ENABLE_GRPC=$ENABLE_GRPC

COPY --from=builder /out /tmp/
COPY setup-node.sh /tmp/
COPY entrypoint.sh /usr/share/waves/bin/

RUN /bin/bash /tmp/setup-node.sh

USER waves
WORKDIR /var/lib/waves
EXPOSE 6869 6868 6863 6862 6870
VOLUME /var/lib/waves
VOLUME /usr/share/waves/lib/plugins

ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
