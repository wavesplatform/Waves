package com.wavesplatform.api

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.api.GrpcConnector.Settings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel

import java.util.concurrent.Executors

class GrpcConnector(settings: Settings) extends AutoCloseable with ScorexLogging {
  private val executor = Executors.newFixedThreadPool(
    settings.exactThreads,
    new ThreadFactoryBuilder().setNameFormat("waves-grpc-%d").setDaemon(false).build()
  )

  def mkChannel(grpcClientSettings: GrpcChannelSettings): ManagedChannel = {
    log.info(s"Creating a channel to ${grpcClientSettings.target}")
    grpcClientSettings.toNettyChannelBuilder
      .executor(executor)
      .usePlaintext()
      .build()
  }

  override def close(): Unit = {
    executor.shutdown()
  }
}

object GrpcConnector {
  case class Settings(executorThreads: Option[Int]) {
    private lazy val defaultThreads = (Runtime.getRuntime.availableProcessors() * 2).min(4)
    val exactThreads                = executorThreads.getOrElse(defaultThreads)
  }
}
