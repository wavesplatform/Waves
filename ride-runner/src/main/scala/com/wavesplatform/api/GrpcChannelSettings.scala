package com.wavesplatform.api

import com.typesafe.config.ConfigMemorySize
import com.wavesplatform.api.GrpcChannelSettings.ChannelOptionsSettings
import io.grpc.netty.{InternalNettyChannelBuilder, NettyChannelBuilder}
import io.netty.channel.ChannelOption

import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.*

final case class GrpcChannelSettings(
    maxHedgedAttempts: Int,
    maxRetryAttempts: Int,
    keepAliveWithoutCalls: Boolean,
    keepAliveTime: FiniteDuration,
    keepAliveTimeout: FiniteDuration,
    idleTimeout: FiniteDuration,
    maxInboundMessageSize: ConfigMemorySize,
    channelOptions: ChannelOptionsSettings,
    maxConcurrentCalls: Option[Int] = None
) {

  def toNettyChannelBuilder(target: String): NettyChannelBuilder =
    NettyChannelBuilder
      .forTarget(target)
      .maxHedgedAttempts(maxHedgedAttempts)
      .maxRetryAttempts(maxRetryAttempts)
      .keepAliveWithoutCalls(keepAliveWithoutCalls)
      .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
      .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
      .maxInboundMessageSize(maxInboundMessageSize.toBytes.toInt)
      .idleTimeout(idleTimeout.length, idleTimeout.unit)
      .withOption[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, channelOptions.connectTimeout.toMillis.toInt)
      .tap { x =>
        InternalNettyChannelBuilder.setStatsEnabled(x, false)
        InternalNettyChannelBuilder.setTracingEnabled(x, false)
      }

}

object GrpcChannelSettings {
  final case class ChannelOptionsSettings(connectTimeout: FiniteDuration)
}
