package com.wavesplatform.block.protobuf

import com.wavesplatform.transaction.protobuf.PBTransactionImplicits._
import com.wavesplatform.{block => vb}

trait PBBlockImplicits {
  implicit class PBBlockConversions(block: Block) {
    def toVanilla: vb.Block = {
      vb.Block(
        block.timestamp,
        block.version.toByte,
        block.reference,
        vb.SignerData(block.getSignerData.generator, block.getSignerData.signature),
        com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData(block.getConsensusData.baseTarget, block.getConsensusData.generationSignature),
        block.transactions.map(_.toVanillaTransaction()),
        block.featureVotes.map(_.toShort)
      )
    }
  }

  implicit class VanillaBlockConversions(block: vb.Block) {
    def toPB: Block = {
      Block(
        block.version,
        block.timestamp,
        block.reference,
        Some(Block.SignerData(block.signerData.generator, block.signerData.signature)),
        Some(Block.ConsensusData(block.consensusData.baseTarget, block.consensusData.generationSignature)),
        block.transactionData.map(_.toPB),
        block.featureVotes.map(_.toInt)
      )
    }
  }
}

object PBBlockImplicits extends PBBlockImplicits
