FROM openjdk:8-jdk-stretch as builder
ARG SBT_VERSION=1.2.8
ARG WAVES_VERSION="latest"
ARG BRANCH="version-1.1.x"
ARG DEB_PACKAGE_NETWORKS

RUN                                                                             \
{                                                                               \
    WAVES_VERSION=$WAVES_VERSION                                            &&  \
    test $WAVES_VERSION = "latest"                                          &&  \
    WAVES_VERSION=$(curl -fsSL                                                  \
        https://api.github.com/repos/wavesplatform/Waves/releases/latest        \
        | tac | grep -m 1 'tag_name.:' | tr -cd '[0-9\.]')                  &&  \
    echo "Using latest version '${WAVES_VERSION}'"                              \
;} ;                                                                            \
{                                                                               \
    curl -fsSL https://api.github.com/repos/wavesplatform/Waves/releases        \
        | tac | grep -q "tag_name.*${WAVES_VERSION}"                        &&  \
    echo "GitHub release '${WAVES_VERSION}' found"                              \
;} &&                                                                           \
{                                                                               \
    echo "Downloading Waves '${WAVES_VERSION}' from GitHub"                 &&  \
    mkdir -p /waves/node/target                                             &&  \
    releaseUrl="https://github.com/wavesplatform/Waves/releases/download"   &&  \
                                                                                \
    echo "Downloading jar file"                                             &&  \
    curl -fL ${releaseUrl}/v${WAVES_VERSION}/waves-all-${WAVES_VERSION}.jar     \
        -o /waves/node/target/waves-all-${WAVES_VERSION}.jar                    \
;} ||                                                                           \
{                                                                               \
    echo "Downloading sbt '${SBT_VERSION}'"                                 &&  \
    curl -fL -o sbt-$SBT_VERSION.deb                                            \
        https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb              &&  \
    dpkg -i sbt-$SBT_VERSION.deb && rm sbt-$SBT_VERSION.deb                 &&  \
    git clone https://github.com/wavesplatform/Waves.git                        \
        --branch $BRANCH waves && cd waves                                  &&  \
    git config --global user.name "Sbt Builder"                             &&  \
    git config --global user.email "sbt@builder.docker"                     &&  \
    git tag -a "v${WAVES_VERSION}" -m "Docker build"                        &&  \
    SBT_OPTS="-Xmx2g -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=128m"       \
    sbt "node/assembly"                                                     &&  \
    for network in $DEB_PACKAGE_NETWORKS ; do                                   \
        echo "Building '${network}' package"                                &&  \
        SBT_OPTS="-XX:ReservedCodeCacheSize=128m                                \
            -Xmx2g -Dnetwork=${network}" sbt 'packageAll'                   &&  \
        mkdir -p /out/${network}                                            &&  \
        mv node/target/waves*.deb /out/${network}/                          &&  \
        cp node/target/waves-all*.jar /out/${network}/                      &&  \
        mv grpc-server/target/universal/grpc-server*.tgz /out/${network}/   &&  \
        mv grpc-server/target/grpc-server*.deb /out/${network}/             ;   \
    done                                                                        \
;}

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
COPY entrypoint.sh /usr/share/waves/bin/

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
