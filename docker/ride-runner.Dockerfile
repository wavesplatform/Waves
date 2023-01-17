# To build: docker build -t wavesplatform/ride-runner:latest -f docker/ride-runner.Dockerfile docker
FROM eclipse-temurin:11-jre

ENV RIDE_LOG_LEVEL=INFO
ENV RIDE_HEAP_SIZE=2g
ENV RIDE_NETWORK=mainnet
ENV RIDE_APP=com.wavesplatform.ride.app.RideWithBlockchainUpdatesService

ENV YOURKIT_VERSION=2022.9

SHELL ["/bin/bash", "-c"]

# Additional dependencies
RUN apt-get update && apt-get install -y wget unzip gosu || exit 1; \
    export YOURKIT_ARCHIVE="YourKit-JavaProfiler-$YOURKIT_VERSION-docker.zip"; \
    wget --quiet "https://www.yourkit.com/download/docker/$YOURKIT_ARCHIVE" -P /tmp/ && unzip /tmp/$YOURKIT_ARCHIVE -d /usr/local || exit 1; \
    # Clean
    apt-get remove -y wget unzip && apt-get autoremove -y && apt-get autoclean && rm -rf /var/lib/apt/lists/*

# RIDE runner files
ENV RDATA=/var/lib/ride-runner
ENV RIDE_INSTALL_PATH=/usr/share/ride-runner
ENV RIDE_CONFIG=/etc/ride-runner/ride-runner.conf
ENV RIDE_LOGBACK_CONFIG=$RIDE_INSTALL_PATH/doc/logback.sample.xml

COPY ride-runner-target /tmp/
COPY ride-runner.conf.template $RIDE_CONFIG

# Setup node
COPY ride-runner.entrypoint.sh $RIDE_INSTALL_PATH/bin/entrypoint.sh
RUN mkdir -p $RDATA; \
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

STOPSIGNAL SIGINT
CMD ["/usr/share/ride-runner/bin/entrypoint.sh"]
