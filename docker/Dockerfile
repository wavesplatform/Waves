FROM openjdk:8-jdk-stretch as builder
ARG WAVES_VERSION="current"
ARG BRANCH="version-1.2.x"
ARG DEB_PACKAGE_NETWORKS

# Run from the root directory: docker build -f docker/Dockerfile .
COPY . waves_current

ENV WAVES_VERSION=$WAVES_VERSION
ENV BRANCH=$BRANCH
ENV DEB_PACKAGE_NETWORKS=$DEB_PACKAGE_NETWORKS
RUN /bin/bash /waves_current/docker/build.sh

FROM openjdk:11-jre-slim
ARG WAVES_VERSION="current"
ARG WAVES_LOG_LEVEL="INFO"
ARG WAVES_HEAP_SIZE="2g"
ARG WAVES_NETWORK="mainnet"
ARG ENABLE_GRPC="false"

ENV WAVES_VERSION=$WAVES_VERSION
ENV WAVES_LOG_LEVEL=$WAVES_LOG_LEVEL
ENV WAVES_HEAP_SIZE=$WAVES_HEAP_SIZE
ENV WAVES_NETWORK=$WAVES_NETWORK
ENV ENABLE_GRPC=$ENABLE_GRPC

COPY --from=builder /waves/node/target/waves-all-*.jar /usr/share/waves/lib/
COPY --from=builder /waves/grpc-server/target/universal /tmp/grpc-server
COPY docker/setup-node.sh /tmp/
COPY docker/entrypoint.sh /usr/share/waves/bin/

RUN /bin/bash /tmp/setup-node.sh

USER waves
WORKDIR /var/lib/waves
EXPOSE 6869 6868 6863 6862 6870
VOLUME /var/lib/waves
VOLUME /usr/share/waves/lib/plugins

ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
