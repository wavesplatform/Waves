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

# Setup node. No USER instruction: entrypoint.sh takes ownership of bind-mounted data and log directories as root, then
# drops to waves (uid 999). Only those two directories are writable by waves; code stays root-owned, so a compromised
# node cannot rewrite what root runs on the next start.
RUN groupadd -r waves --gid=999; \
    useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves; \
    install -d -o waves -g waves -m 755 $WVDATA $WVLOG; \
    install -d -m 755 $WAVES_INSTALL_PATH


# Ahead of the COPYs below: it is the only instruction left that touches the filesystem, and running it after them
# would force BuildKit to materialize the merged --link layers instead of ending the build with them.
WORKDIR $WVDATA

# `sbt stageForDocker` lays the node and grpc-server files out under target/: third-party jars in lib/, the ones that
# exist per platform in native/<arch>/, Waves' own jars and scripts in app/. Copying them as separate layers keeps the
# big one cached across builds that only changed Waves code, and copying (rather than unpacking a tarball in a RUN)
# needs no emulation on a cross-platform build. Nothing is downloaded at build time.
# --chmod: the node runs as waves, and stageForDocker never refreshes the mode of a staged file it considers unchanged,
# so a stale 0600 jar in docker/target would otherwise reach the image unreadable. X keeps directories and scripts executable.
COPY --link --chmod=u=rwX,go=rX target/lib $WAVES_INSTALL_PATH/lib
COPY --link --chmod=u=rwX,go=rX target/native/$TARGETARCH $WAVES_INSTALL_PATH/lib
COPY --link --chmod=u=rwX,go=rX target/app $WAVES_INSTALL_PATH
COPY --link --chmod=755 entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh


EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins

HEALTHCHECK CMD curl -f http://localhost:6869/node/status || exit 1

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
