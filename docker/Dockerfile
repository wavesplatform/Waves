ARG baseImage=eclipse-temurin:21-jre-noble
FROM $baseImage

ARG TARGETARCH

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

# Setup node. There is deliberately no USER instruction: entrypoint.sh starts as root only to take ownership of a
# bind-mounted data or log directory, then re-execs itself as waves (uid 999) with setpriv, from util-linux in the
# base image. Declaring USER instead would leave every bind mount for the operator to chown by hand.
RUN groupadd -r waves --gid=999; \
    useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves; \
    # The COPYs below own what they bring in, so only the directories themselves are created here
    install -d -o waves -g waves -m 755 $WVDATA $WVLOG $WAVES_INSTALL_PATH

# Ahead of the COPYs below: it is the only instruction left that touches the filesystem, and running it after them
# would force BuildKit to materialize the merged --link layers instead of ending the build with them.
WORKDIR $WVDATA

# `sbt stageForDocker` lays the node and grpc-server files out under target/: third-party jars in lib/, the ones that
# exist per platform in native/<arch>/, Waves' own jars and scripts in app/. Copying them as separate layers keeps the
# big one cached across builds that only changed Waves code, and copying (rather than unpacking a tarball in a RUN)
# needs no emulation on a cross-platform build. Nothing is downloaded at build time.
# --chown is numeric because --link copies into a layer that has no user database of its own.
COPY --link --chown=999:999 target/lib $WAVES_INSTALL_PATH/lib
COPY --link --chown=999:999 target/native/$TARGETARCH $WAVES_INSTALL_PATH/lib
COPY --link --chown=999:999 target/app $WAVES_INSTALL_PATH
COPY --link --chown=999:999 entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh

EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins

HEALTHCHECK CMD curl -f http://localhost:6869/node/status || exit 1

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
