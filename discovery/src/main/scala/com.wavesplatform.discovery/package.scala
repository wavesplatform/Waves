package com.wavesplatform

import io.netty.channel.nio.NioEventLoopGroup

import scala.concurrent.ExecutionContext

package object discovery {

  implicit val workerGroup = new NioEventLoopGroup
  implicit val ec: ExecutionContext = ExecutionContext.global

}
