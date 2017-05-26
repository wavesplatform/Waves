package com.wavesplatform.network

import com.wavesplatform.utils.ByteStr
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter}
import io.netty.util.concurrent.ScheduledFuture
import scorex.block.Block
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.encode.Base58.encode
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration

class ExtensionBlocksLoader(history: History, blockSyncTimeout: FiniteDuration)
    extends ChannelInboundHandlerAdapter with ScorexLogging {
  private var pendingSignatures = Map.empty[ByteStr, Int]
  private var targetExtensionIds = Option.empty[ExtensionIds]
  private val blockBuffer = mutable.TreeMap.empty[Int, Block]
  private var currentTimeout = Option.empty[ScheduledFuture[_]]

  private def cancelTimeout(): Unit = {
    currentTimeout.foreach(_.cancel(false))
    currentTimeout = None
  }

  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef) = msg match {
    case xid@ExtensionIds(_, newIds) if pendingSignatures.isEmpty =>
      targetExtensionIds = Some(xid)
      pendingSignatures = newIds.zipWithIndex.toMap
      cancelTimeout()
      currentTimeout = Some(ctx.executor().schedule(blockSyncTimeout) {
        if (targetExtensionIds.contains(xid)) {
          log.warn(s"${ctx.channel().id().asShortText()}: timeout loading blocks")
          // todo: blacklist?
        }
      })
      newIds.foreach(s => ctx.write(GetBlock(s)))
      ctx.flush()

    case b: Block if pendingSignatures.contains(ByteStr(b.uniqueId)) =>
      val signature = ByteStr(b.uniqueId)
      blockBuffer += pendingSignatures(signature) -> b
      pendingSignatures -= signature
      if (pendingSignatures.isEmpty) {
        cancelTimeout()
        log.debug(s"${ctx.channel().id().asShortText()}: Loaded all blocks, doing a pre-check")

        val newBlocks = blockBuffer.values.toSeq

        for (tids <- targetExtensionIds) {
          if (!(tids.lastCommonId == ByteStr(newBlocks.head.reference))) {
            log.warn(s"${ctx.channel().id().asShortText()}: Extension head reference ${encode(newBlocks.head.reference)} differs from last common block id ${tids.lastCommonId}")
            // todo: blacklist?
          } else if (!newBlocks.sliding(2).forall {
              case Seq(b1, b2) => b1.uniqueId.sameElements(b2.reference)
              case _ => true
            }) {
            log.warn(s"${ctx.channel().id().asShortText()}: Extension blocks are not contiguous, pre-check failed")
            // todo: blacklist?
          } else {
            val localScore = history.score()
            val forkScore = newBlocks.view.map(_.blockScore).foldLeft(history.scoreOf(tids.lastCommonId.bytes))(_ + _)

            if (forkScore <= localScore) {
              log.debug(s"${ctx.channel().id().asShortText()}: Fork score $forkScore is not higher than local score $localScore, pre-check failed")
            } else {
              newBlocks.par.find(!blockIsValid(_)) match {
                case Some(invalidBlock) =>
                  log.warn(s"${ctx.channel().id().asShortText()}: Got block ${Base58.encode(invalidBlock.uniqueId)} with invalid signature")
                case None =>
                  log.debug(s"${ctx.channel().id().asShortText()}: Chain is valid, pre-check passed")
                  ctx.fireChannelRead(ExtensionBlocks(newBlocks))
              }
            }
          }
        }

        targetExtensionIds = None
        blockBuffer.clear()
      }

    case Signatures(sigs) =>
      log.warn(s"R${ctx.channel().id().asShortText()}: received unexpected extension ids while loading blocks, ignoring")
    case _ => super.channelRead(ctx, msg)
  }

  private def blockIsValid(b: Block) =
    EllipticCurveImpl.verify(b.signerData.signature, b.bytesWithoutSignature, b.signerData.generator.publicKey)
}
