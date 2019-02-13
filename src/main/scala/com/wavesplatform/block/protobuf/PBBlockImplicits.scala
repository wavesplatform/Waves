package com.wavesplatform.block.protobuf

import com.wavesplatform.transaction.protobuf.Transaction._
import com.wavesplatform.{block => vb}

trait PBBlockImplicits {
  implicit class PBBlockImplicitConversionOps(block: Block) {
    def toVanilla: vb.Block = {
      vb.Block(
        block.timestamp,
        block.version.toByte,
        block.reference,
        vb.SignerData(block.getSignerData.generator, block.getSignerData.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getConsensusData.baseTarget, block.getConsensusData.generationSignature),
        block.transactions.map(_.toVanilla),
        block.featureVotes.map(toShortWithCheck)
      )
    }
  }

  implicit class VanillaBlockConversions(block: vb.Block) {
    def toPB: Block = {
      Block(
        block.reference,
        Some(Block.SignerData(block.signerData.generator, block.signerData.signature)),
        Some(Block.ConsensusData(block.consensusData.baseTarget, block.consensusData.generationSignature)),
        block.transactionData.map(_.toPB),
        block.featureVotes.map(_.toInt),
        block.timestamp,
        block.version
      )
    }
  }

  private[this] def toShortWithCheck(int: Int): Short = {
    require(int > 0 && int < 65535, s"Short overflow: $int")
    int.toShort
  }
}

// object PBBlockImplicits extends PBBlockImplicits
