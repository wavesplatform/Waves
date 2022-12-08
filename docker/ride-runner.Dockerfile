FROM openjdk:11-jre-slim

ENV WAVES_LOG_LEVEL=INFO
ENV WAVES_HEAP_SIZE=2g
ENV WAVES_NETWORK=mainnet

ENV YOURKIT_VERSION=2022.9

SHELL ["/bin/bash", "-c"]

# Additional dependencies
RUN apt-get update && apt-get install -y wget unzip gosu || exit 1; \
    export YOURKIT_ARCHIVE="YourKit-JavaProfiler-$YOURKIT_VERSION-docker.zip"; \
    wget --quiet "https://www.yourkit.com/download/docker/$YOURKIT_ARCHIVE" -P /tmp/ && unzip /tmp/$YOURKIT_ARCHIVE -d /usr/local || exit 1; \
    # Clean
    apt-get remove -y wget unzip && apt-get autoremove -y && apt-get autoclean && rm -rf /var/lib/apt/lists/*

# RIDE runner files
ENV WVDATA=/var/lib/ride-runner
ENV WVLOG=/var/log/ride-runner
ENV WAVES_INSTALL_PATH=/usr/share/ride-runner
ENV WAVES_CONFIG=/etc/ride-runner/ride-runner.conf

COPY target /tmp/
COPY ride-runner.conf.template $WAVES_CONFIG

# Setup node
COPY ride-runner.entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh
RUN mkdir -p $WVDATA $WVLOG; \
    # Create user
    groupadd -r ride --gid=999; \
    useradd -r -g ride --uid=999 --home-dir=$WVDATA --shell=/bin/bash ride; \
    # Unpack tgz packages
    tar zxvf /tmp/ride-runner.tgz -C $WAVES_INSTALL_PATH --strip-components=1; \
    # Set permissions
    chown -R ride:ride $WVDATA $WVLOG $WAVES_INSTALL_PATH && chmod 755 $WVDATA $WVLOG; \
    # Cleanup
    rm -rf /tmp/*

EXPOSE 6890
VOLUME $WVDATA
VOLUME $WVLOG
WORKDIR $WVDATA

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/ride-runner/bin/entrypoint.sh"]
