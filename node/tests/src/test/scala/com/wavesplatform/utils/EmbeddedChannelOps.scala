package com.wavesplatform.utils

import com.wavesplatform.network.{EndorseBlock, EndorseBlockSpec, RawBytes}
import io.netty.channel.embedded.EmbeddedChannel
import org.scalactic.source.Position
import org.scalatest.Suite
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters.CollectionHasAsScala

trait EmbeddedChannelOps { suite: Suite & Matchers =>
  extension (self: EmbeddedChannel) {
    def sentEndorsements: Seq[EndorseBlock] = {
      val xs = self
        .outboundMessages()
        .asScala
        .collect {
          case x: RawBytes if x.code == EndorseBlockSpec.messageCode => EndorseBlockSpec.deserializeData(x.data).get
        }
        .toSeq
      self.outboundMessages().clear()
      xs
    }

    def sentOneEndorsement(using Position): Option[EndorseBlock] = {
      val xs = sentEndorsements

      withClue("sent only one endorsement: ") {
        xs.size should be <= 1
      }
      xs.headOption
    }
  }
}
