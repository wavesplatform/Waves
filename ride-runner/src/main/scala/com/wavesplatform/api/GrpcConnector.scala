package com.wavesplatform.api

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

class GrpcConnector(executorThreads: Int) extends AutoCloseable with ScorexLogging {
  private val executor = new ThreadPoolExecutor(
    1,
    executorThreads,
    60,
    TimeUnit.SECONDS,
    new LinkedBlockingQueue[Runnable],
    new ThreadFactoryBuilder().setNameFormat("waves-grpc-%d").setDaemon(false).build()
  )

  def mkChannel(target: String, grpcClientSettings: GrpcChannelSettings): ManagedChannel = {
    log.info(s"Creating a channel to $target with settings: $grpcClientSettings")
    grpcClientSettings
      .toNettyChannelBuilder(target)
      .executor(executor)
      .usePlaintext()
      .build()
  }

  override def close(): Unit = executor.shutdown()
}
