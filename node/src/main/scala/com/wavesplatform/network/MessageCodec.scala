package com.wavesplatform.network

import java.util

import com.wavesplatform.crypto
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.ChannelHandlerContext
import io.netty.handler.codec.MessageToMessageCodec

import scala.util.{Failure, Success}

@Sharable
class MessageCodec(peerDatabase: PeerDatabase) extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  import BasicMessagesRepo.specsByCodes

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]): Unit = msg match {
    // Have no spec
    case r: RawBytes              => out.add(r)
    case LocalScoreChanged(score) => out.add(RawBytes(ScoreSpec.messageCode, ScoreSpec.serializeData(score)))
    case BlockForged(b)           => out.add(RawBytes.fromBlock(b))

    // With a spec
    case GetPeers                => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers           => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case g: GetBlock             => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case m: MicroBlockInv        => out.add(RawBytes(MicroBlockInvSpec.messageCode, MicroBlockInvSpec.serializeData(m)))
    case m: MicroBlockRequest    => out.add(RawBytes(MicroBlockRequestSpec.messageCode, MicroBlockRequestSpec.serializeData(m)))
    case g: GetSnapshot          => out.add(RawBytes(GetSnapsnotSpec.messageCode, GetSnapsnotSpec.serializeData(g)))
    case m: MicroSnapshotRequest => out.add(RawBytes(MicroSnapshotRequestSpec.messageCode, MicroSnapshotRequestSpec.serializeData(m)))
    case s: Snapshots            => out.add(RawBytes(SnapshotsSpec.messageCode, SnapshotsSpec.serializeData(s)))

    // Version switch
    case gs: GetSignatures if isNewMsgsSupported(ctx) =>
      out.add(RawBytes(GetBlockIdsSpec.messageCode, GetBlockIdsSpec.serializeData(gs)))
    case gs: GetSignatures if GetSignaturesSpec.isSupported(gs.signatures) =>
      out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))

    case s: Signatures =>
      if (isNewMsgsSupported(ctx)) {
        out.add(RawBytes(BlockIdsSpec.messageCode, BlockIdsSpec.serializeData(s)))
      } else {
        val supported = s.signatures
          .dropWhile(_.arr.length != crypto.SignatureLength)
          .takeWhile(_.arr.length == crypto.SignatureLength)
        out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s.copy(signatures = supported))))
      }

    case _ =>
      throw new IllegalArgumentException(s"Can't send message $msg to $ctx (unsupported)")
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
