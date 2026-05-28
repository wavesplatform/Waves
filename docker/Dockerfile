ARG baseImage=eclipse-temurin:21-jre-noble
FROM $baseImage

ARG TARGETARCH
ARG CORRETTO_VERSION=2.5.0
ARG ROCKSDB_VERSION=10.2.1

ENV WAVES_LOG_LEVEL=INFO
ENV WAVES_LOG_JAVA_OPTS=true

ENV WAVES_HEAP_SIZE=2g
ENV WAVES_NETWORK=mainnet

SHELL ["/bin/bash", "-c"]

# Node files
ENV WVDATA=/var/lib/waves
ENV WVLOG=/var/log/waves
ENV WAVES_INSTALL_PATH=/usr/share/waves
ENV WAVES_CONFIG=/etc/waves/waves.conf

RUN if [ "$TARGETARCH" = "amd64" ]; then \
        export CORRETTO_ARCH="linux-x86_64"; \
    elif [ "$TARGETARCH" = "arm64" ]; then \
        export CORRETTO_ARCH="linux-aarch_64"; \
    fi && \
    export CORRETTO_JAR="AmazonCorrettoCryptoProvider-${CORRETTO_VERSION}-${CORRETTO_ARCH}.jar" && \
    export ROCKSDB_JAR="rocksdbjni-${ROCKSDB_VERSION}-linux64.jar" && \
    mkdir -p "${WAVES_INSTALL_PATH}/lib" && \
    wget "https://repo1.maven.org/maven2/software/amazon/cryptools/AmazonCorrettoCryptoProvider/${CORRETTO_VERSION}/${CORRETTO_JAR}" -O "${WAVES_INSTALL_PATH}/lib/software.amazon.cryptools.${CORRETTO_JAR}" && \
    wget "https://repo1.maven.org/maven2/org/rocksdb/rocksdbjni/${ROCKSDB_VERSION}/${ROCKSDB_JAR}" -O "${WAVES_INSTALL_PATH}/lib/org.rocksdb.${ROCKSDB_JAR}"

# Setup node
COPY entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh
RUN mkdir -p $WVDATA $WVLOG; \
    # Create user
    groupadd -r waves --gid=999; \
    useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves

RUN --mount=type=bind,source=target,target=/tmp/ \
    # Unpack tgz packages
    tar zxvf /tmp/waves.tgz -C $WAVES_INSTALL_PATH --strip-components=1 --exclude '*AmazonCorrettoCryptoProvider*.jar' --exclude '*conscrypt*.jar' --exclude '*rocksdbjni*' && \
    tar zxvf /tmp/waves-grpc-server.tgz -C $WAVES_INSTALL_PATH --strip-components=1 && \
    # Set permissions
    chown -R waves:waves $WVDATA $WVLOG $WAVES_INSTALL_PATH && chmod 755 $WVDATA $WVLOG;

EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins
WORKDIR $WVDATA

HEALTHCHECK CMD curl -f http://localhost:6869/node/status || exit 1

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
