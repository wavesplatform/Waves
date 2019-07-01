package com.wavesplatform.protobuf.block

import com.wavesplatform.block.{BlockHeader, SignerData}
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.protobuf.utils.PBImplicitConversions._
import monix.eval.Coeval

import scala.annotation.switch

class PBBlockAdapter(block: PBCachedBlock)
    extends VanillaBlock(
      block.block.getHeader.timestamp,
      block.block.getHeader.version.toByte,
      block.block.getHeader.reference,
      SignerData(block.block.getHeader.generator.publicKey, block.block.signature),
      NxtLikeConsensusBlockData(block.block.getHeader.baseTarget, block.block.getHeader.generationSignature),
      block.transactions.map(PBTransactions.vanilla(_, unsafe = true)),
      block.block.getHeader.featureVotes
    ) {
  def isLegacy: Boolean = (this.version: @switch) match {
    case 1 | 2 | 3 => true
    case _         => false
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) super.bytes()
    else block.bytes
  )

  override val bytesWithoutSignature: Coeval[Array[Byte]] = Coeval.evalOnce(
    if (isLegacy) super.bytesWithoutSignature()
    else block.headerBytes
  )

  override def getHeader(): BlockHeader =
    new PBBlockAdapter(block.withTransactions(Nil))
}

object PBBlockAdapter {
  def apply(block: PBCachedBlock): PBBlockAdapter = new PBBlockAdapter(block)
}
