FROM openjdk:8-jdk-stretch as builder
ARG WAVES_VERSION="latest"
ARG BRANCH="version-1.1.x"
ARG DEB_PACKAGE_NETWORKS

# Run from the root directory: docker build -f docker/Dockerfile .
COPY . waves_current

ENV WAVES_VERSION=$WAVES_VERSION
ENV BRANCH=$BRANCH
ENV DEB_PACKAGE_NETWORKS=$DEB_PACKAGE_NETWORKS
RUN chmod +x /waves_current/docker/build.sh && /waves_current/docker/build.sh

FROM openjdk:11-jre-slim
ARG WAVES_VERSION="latest"
ARG WAVES_LOG_LEVEL="INFO"
ARG WAVES_HEAP_SIZE="2g"
ARG WAVES_NETWORK="mainnet"

ENV WAVES_VERSION=$WAVES_VERSION
ENV WAVES_LOG_LEVEL=$WAVES_LOG_LEVEL
ENV WAVES_HEAP_SIZE=$WAVES_HEAP_SIZE
ENV WAVES_NETWORK=$WAVES_NETWORK

COPY --from=builder /waves/node/target/waves-all-*.jar /usr/share/waves/lib/
COPY docker/entrypoint.sh /usr/share/waves/bin/

RUN groupadd -g 143 waves                                                   &&  \
    useradd -d /var/lib/waves -g 143 -u 143 -s /bin/bash -M waves           &&  \
    mkdir -p /var/lib/waves /etc/waves /usr/share/waves/lib/plugins         &&  \
    chown -R 143:143 /var/lib/waves /usr/share/waves /etc/waves             &&  \
    chmod -R 755 /var/lib/waves /usr/share/waves /etc/waves                 &&  \
    ln -fs /var/lib/waves/log /var/log/waves

WORKDIR /var/lib/waves
EXPOSE 6869 6868 6863 6862

USER waves
VOLUME /var/lib/waves
VOLUME /usr/share/waves/lib/plugins
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
