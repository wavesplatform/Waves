package com.wavesplatform.grpc

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.grpc.GrpcConnector.Settings
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel

import java.util.concurrent.Executors

class GrpcConnector(settings: Settings) extends AutoCloseable with ScorexLogging {
  private val eventLoopGroup = new NioEventLoopGroup(settings.exactEventLoopThreads)
  private val executor = Executors.newFixedThreadPool(
    settings.exactThreads,
    new ThreadFactoryBuilder().setNameFormat("waves-grpc-%d").setDaemon(false).build()
  )

  def mkChannel(grpcClientSettings: GrpcClientSettings): ManagedChannel = {
    log.info(s"Creating a channel to ${grpcClientSettings.target}")
    grpcClientSettings.toNettyChannelBuilder
      .executor(executor)
      .eventLoopGroup(eventLoopGroup)
      .channelType(classOf[NioSocketChannel])
      .usePlaintext()
      .build()
  }

  override def close(): Unit = {
    eventLoopGroup.shutdownGracefully()
    executor.shutdown()
  }
}

object GrpcConnector {
  case class Settings(eventLoopThreads: Option[Int], executorThreads: Option[Int]) {
    private lazy val defaultThreads = (Runtime.getRuntime.availableProcessors() * 2).min(4)
    val exactEventLoopThreads       = eventLoopThreads.getOrElse(defaultThreads)
    val exactThreads                = executorThreads.getOrElse(defaultThreads)
  }
}
