package com.wavesplatform.api

import com.wavesplatform.api.GrpcChannelSettings.ChannelOptionsSettings
import io.grpc.netty.{InternalNettyChannelBuilder, NettyChannelBuilder}
import io.netty.channel.ChannelOption

import scala.concurrent.duration.FiniteDuration
import scala.util.chaining.*

final case class GrpcChannelSettings(
    target: String,
    maxHedgedAttempts: Int,
    maxRetryAttempts: Int,
    keepAliveWithoutCalls: Boolean,
    keepAliveTime: FiniteDuration,
    keepAliveTimeout: FiniteDuration,
    idleTimeout: FiniteDuration,
    maxInboundMessageSize: Int,
    channelOptions: ChannelOptionsSettings
) {

  def toNettyChannelBuilder: NettyChannelBuilder =
    NettyChannelBuilder
      .forTarget(target)
      .maxHedgedAttempts(maxHedgedAttempts)
      .maxRetryAttempts(maxRetryAttempts)
      .keepAliveWithoutCalls(keepAliveWithoutCalls)
      .keepAliveTime(keepAliveTime.length, keepAliveTime.unit)
      .keepAliveTimeout(keepAliveTimeout.length, keepAliveTimeout.unit)
      .maxInboundMessageSize(maxInboundMessageSize)
      .idleTimeout(idleTimeout.length, idleTimeout.unit)
      .withOption[Integer](ChannelOption.CONNECT_TIMEOUT_MILLIS, channelOptions.connectTimeout.toMillis.toInt)
      .tap { x =>
        // TODO #102 gRPC stats and tracing
        InternalNettyChannelBuilder.setStatsEnabled(x, false)
        InternalNettyChannelBuilder.setTracingEnabled(x, false)
      }

}

object GrpcChannelSettings {
  final case class ChannelOptionsSettings(connectTimeout: FiniteDuration)
}