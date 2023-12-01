FROM eclipse-temurin:8-jdk-jammy

ARG WAVES_VERSION
ARG SBT_VERSION

ENV PATH="/opt/sbt/bin:$PATH"
ENV HOME=/opt/sbt/home

RUN curl -L https://github.com/sbt/sbt/releases/download/v$SBT_VERSION/sbt-$SBT_VERSION.tgz | tar -xzf - -C /opt

RUN apt-get update && apt-get -y install git

VOLUME /src

RUN git clone --depth 1 -b v$WAVES_VERSION https://github.com/wavesplatform/Waves.git /src && \
  cd /src && \
  sbt --batch --mem 2048 ";node/compile;grpc-server/compile"
