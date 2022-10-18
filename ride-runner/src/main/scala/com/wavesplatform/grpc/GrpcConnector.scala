package com.wavesplatform.grpc

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.utils.ScorexLogging
import io.grpc.ManagedChannel
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.nio.NioSocketChannel

import java.util.concurrent.Executors

class GrpcConnector extends AutoCloseable with ScorexLogging {
  private val eventLoopGroup = new NioEventLoopGroup(2)

  private val executor =
    Executors.newFixedThreadPool(2, new ThreadFactoryBuilder().setNameFormat("waves-grpc-%d").setDaemon(false).build())

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
