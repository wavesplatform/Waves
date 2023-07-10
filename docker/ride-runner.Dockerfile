# To build: docker build -t wavesplatform/ride-runner:latest -f docker/ride-runner.Dockerfile docker
# JDK for now, because I need jmap, e.g. ijmap -dump:format=b,file=heap-dump.hprof
FROM eclipse-temurin:17-jdk

# /usr/local/async-profiler/build/libasyncProfiler.so
ENV ASYNCPROF_VERSION=2.9

SHELL ["/bin/bash", "-c"]

# Additional dependencies
RUN apt-get update && apt-get install -y wget unzip gosu || exit 1; \
    export ASYNCPROF_ARCHIVE="async-profiler-${ASYNCPROF_VERSION}-linux-x64.tar.gz"; \
    wget --quiet "https://github.com/jvm-profiling-tools/async-profiler/releases/download/v${ASYNCPROF_VERSION}/${ASYNCPROF_ARCHIVE}" -P /tmp/ && \
      mkdir /usr/local/async-profiler; tar -xvzf /tmp/$ASYNCPROF_ARCHIVE --strip-components=1 -C /usr/local/async-profiler || exit 1; \
    # Clean
    apt-get remove -y wget unzip && apt-get autoremove -y && apt-get autoclean && rm -rf /var/lib/apt/lists/*

ENV RIDE_LOG_LEVEL=INFO
ENV RIDE_HEAP_SIZE=2500m
ENV RIDE_NETWORK=mainnet
ENV RIDE_APP=com.wavesplatform.ride.runner.entrypoints.RideRunnerWithBlockchainUpdatesService

# RIDE runner files
ENV RDATA=/var/lib/ride-runner
ENV RIDE_INSTALL_PATH=/usr/share/ride-runner
ENV RIDE_CONFIG=/etc/ride-runner/ride-runner.conf
ENV RIDE_LOGBACK_CONFIG=$RIDE_INSTALL_PATH/doc/logback.sample.xml

COPY ride-runner-target /tmp/
COPY ride-runner.conf.template $RIDE_CONFIG

# Setup node
COPY ride-runner.entrypoint.sh $RIDE_INSTALL_PATH/bin/entrypoint.sh
RUN mkdir -p $RDATA/heap-dumps/on-exit; \
    # Create user
    groupadd -r ride --gid=999; \
    useradd -r -g ride --uid=999 --home-dir=$RDATA --shell=/bin/bash ride; \
    # Unpack tgz packages
    tar zxvf /tmp/ride-runner.tgz -C $RIDE_INSTALL_PATH --strip-components=1; \
    # Set permissions
    chown -R ride:ride $RDATA $RIDE_INSTALL_PATH && chmod 755 $RDATA; \
    # Cleanup
    rm -rf /tmp/*

EXPOSE 6890 9095
VOLUME $RDATA
WORKDIR $RDATA

HEALTHCHECK CMD curl -f http://localhost:6890/ride/status || exit 1

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/ride-runner/bin/entrypoint.sh"]
