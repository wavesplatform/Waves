package com.wavesplatform.network

import java.util

import com.wavesplatform.crypto
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec

import scala.util.{Failure, Success}

@Sharable
class MessageCodecL1(peerDatabase: PeerDatabase) extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  import BasicMessagesRepo.specsByCodes

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]): Unit = {
    val encodedMsg = msg match {
      // Have no spec
      case r: RawBytes              => r
      case LocalScoreChanged(score) => RawBytes.from(ScoreSpec, score)
      case BlockForged(b)           => RawBytes.fromBlock(b)

      // With a spec
      case GetPeers                      => RawBytes.from(GetPeersSpec, GetPeers)
      case k: KnownPeers                 => RawBytes.from(PeersSpec, k)
      case g: GetBlock                   => RawBytes.from(GetBlockSpec, g)
      case m: MicroBlockInv              => RawBytes.from(MicroBlockInvSpec, m)
      case m: MicroBlockRequest          => RawBytes.from(MicroBlockRequestSpec, m)
      case g: GetSnapshot                => RawBytes.from(GetSnapsnotSpec, g)
      case m: MicroSnapshotRequest       => RawBytes.from(MicroSnapshotRequestSpec, m)
      case s: BlockSnapshotResponse      => RawBytes.from(BlockSnapshotResponseSpec, s)
      case s: MicroBlockSnapshotResponse => RawBytes.from(MicroBlockSnapshotResponseSpec, s)

      // Version switch
      case gs: GetSignatures if isNewMsgsSupported(ctx) =>
        RawBytes.from(GetBlockIdsSpec, gs)
      case gs: GetSignatures if GetSignaturesSpec.isSupported(gs.signatures) =>
        RawBytes.from(GetSignaturesSpec, gs)

      case s: Signatures =>
        if (isNewMsgsSupported(ctx)) {
          RawBytes.from(BlockIdsSpec, s)
        } else {
          val supported = s.signatures
            .dropWhile(_.arr.length != crypto.SignatureLength)
            .takeWhile(_.arr.length == crypto.SignatureLength)
          RawBytes.from(SignaturesSpec, s.copy(signatures = supported))
        }

      case _ =>
        throw new IllegalArgumentException(s"Can't send message $msg to $ctx (unsupported)")
    }

    out.add(encodedMsg)
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    specsByCodes(msg.code).deserializeData(msg.data) match {
      case Success(x) => out.add(x)
      case Failure(e) => block(ctx, e)
    }
  }

  protected def block(ctx: ChannelHandlerContext, e: Throwable): Unit = {
    peerDatabase.blacklistAndClose(ctx.channel(), s"Invalid message. ${e.getMessage}")
  }

  private[this] def isNewMsgsSupported(ctx: ChannelHandlerContext): Boolean = {
    val (v1, v2, _) = ctx.channel().attr(HandshakeHandler.NodeVersionAttributeKey).get()
    v1 > 1 || (v1 == 1 && v2 >= 2) // >= 1.2.0
  }
}
