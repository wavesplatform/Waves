package com.wavesplatform.network

import java.util.concurrent.ConcurrentHashMap

import com.wavesplatform.Version
import com.wavesplatform.test.FreeSpec
import io.netty.buffer.{ByteBuf, Unpooled}
import io.netty.channel.Channel
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.group.ChannelGroup
import org.scalamock.scalatest.MockFactory

import scala.concurrent.duration.DurationInt
import scala.util.Random

class ClientSpec extends FreeSpec with MockFactory {

  private val clientHandshake = new Handshake(
    applicationName = "wavesI",
    applicationVersion = Version.VersionTuple,
    nodeName = "test",
    nodeNonce = Random.nextInt(),
    declaredAddress = None
  )

  private val serverHandshake = clientHandshake.copy(nodeNonce = Random.nextInt())

  "should send only a local handshake on connection" in {
    val channel = createEmbeddedChannel(mock[ChannelGroup])

    val sentClientHandshakeBuff = channel.readOutbound[ByteBuf]()
    Handshake.decode(sentClientHandshakeBuff) shouldBe clientHandshake
    channel.outboundMessages() shouldBe empty
  }

  "should add a server's channel to all channels after the handshake only" in {
    var channelWasAdded = false
    val allChannels     = mock[ChannelGroup]
    (allChannels.add _).expects(*).onCall { (_: Channel) =>
      channelWasAdded = true
      true
    }

    val channel = createEmbeddedChannel(allChannels)

    // skip the client's handshake
    channel.readOutbound[ByteBuf]()
    channelWasAdded shouldBe false

    val replyServerHandshakeBuff = Unpooled.buffer()
    serverHandshake.encode(replyServerHandshakeBuff)
    channel.writeInbound(replyServerHandshakeBuff)
    channelWasAdded shouldBe true
  }

  private def createEmbeddedChannel(allChannels: ChannelGroup) = new EmbeddedChannel(
    new HandshakeDecoder(PeerDatabase.NoOp),
    new HandshakeTimeoutHandler(1.minute),
    new HandshakeHandler.Client(
      handshake = clientHandshake,
      establishedConnections = new ConcurrentHashMap(),
      peerConnections = new ConcurrentHashMap(),
      peerDatabase = PeerDatabase.NoOp,
      allChannels = allChannels
    )
  )

}
