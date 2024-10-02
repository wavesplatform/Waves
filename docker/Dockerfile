FROM eclipse-temurin:11-jre-noble

ENV WAVES_LOG_LEVEL=INFO
ENV WAVES_HEAP_SIZE=2g
ENV WAVES_NETWORK=mainnet

SHELL ["/bin/bash", "-c"]

# Node files
ENV WVDATA=/var/lib/waves
ENV WVLOG=/var/log/waves
ENV WAVES_INSTALL_PATH=/usr/share/waves
ENV WAVES_CONFIG=/etc/waves/waves.conf
COPY target /tmp/

# Setup node
COPY entrypoint.sh $WAVES_INSTALL_PATH/bin/entrypoint.sh
ARG INCLUDE_GRPC=true
RUN mkdir -p $WVDATA $WVLOG; \
    # Create user
    groupadd -r waves --gid=999; \
    useradd -r -g waves --uid=999 --home-dir=$WVDATA --shell=/bin/bash waves; \
    # Unpack tgz packages
    tar zxvf /tmp/waves.tgz -C $WAVES_INSTALL_PATH --strip-components=1; \
    if [[ $INCLUDE_GRPC == true ]]; then \
      echo "Unpacking gRPC server"; \
      tar zxvf /tmp/waves-grpc-server.tgz -C $WAVES_INSTALL_PATH --strip-components=1; \
    fi; \
    # Set permissions
    chown -R waves:waves $WVDATA $WVLOG $WAVES_INSTALL_PATH && chmod 755 $WVDATA $WVLOG; \
    # Cleanup
    rm -rf /tmp/*

EXPOSE 6869 6868 6863 6862 6870
VOLUME $WVDATA
VOLUME $WVLOG
VOLUME $WAVES_INSTALL_PATH/lib/plugins
WORKDIR $WVDATA

HEALTHCHECK CMD curl -f http://localhost:6869/node/status || exit 1

STOPSIGNAL SIGINT
ENTRYPOINT ["/usr/share/waves/bin/entrypoint.sh"]
