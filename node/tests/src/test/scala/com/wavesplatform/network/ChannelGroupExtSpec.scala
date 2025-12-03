package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.test.FreeSpec
import io.netty.channel.*
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import org.scalamock.scalatest.MockFactory

import scala.jdk.CollectionConverters.*
import scala.util.Random

class ChannelGroupExtSpec extends FreeSpec with MockFactory {
  "broadcast" - {
    "should not send a message to the excluded channels" in {
      val message = "test"

      val channelGroup = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val received     = ConcurrentHashMap.newKeySet[Int]()

      def receiver(id: Int): Channel = new EmbeddedChannel(
        new ChannelId {
          override def asShortText(): String        = asLongText()
          override def asLongText(): String         = id.toString
          override def compareTo(o: ChannelId): Int = o.asLongText().toInt - id
        },
        new ChannelOutboundHandlerAdapter {
          override def write(ctx: ChannelHandlerContext, msg: scala.Any, promise: ChannelPromise): Unit = {
            received.add(id)
            super.write(ctx, msg, promise)
          }
        }
      )

      val allIds      = (0 to 5).toSet
      val allChannels = allIds.map(receiver)

      val excludedChannels = allChannels.filter(_ => Random.nextBoolean())
      val excludedIds      = excludedChannels.map(_.id.asLongText().toInt)

      allChannels.foreach(channelGroup.add)
      channelGroup.broadcast(message, excludedChannels).syncUninterruptibly()

      received.asScala shouldBe (allIds -- excludedIds)
    }
  }
}
