package com.wavesplatform.network

import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerAdapter

@Sharable
class NoopHandler extends ChannelHandlerAdapter
