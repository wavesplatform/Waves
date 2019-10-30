package com.wavesplatform.consensus

import cats.implicits._
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.{BlockchainSettings, SynchronizationSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{BaseTargetReachedMaximum, ScorexLogging, forceStopApplication}

import scala.concurrent.duration.FiniteDuration

class PoSSelector(blockchain: Blockchain, blockchainSettings: BlockchainSettings, syncSettings: SynchronizationSettings) extends ScorexLogging {
  import PoSCalculator._

  protected def pos(height: Int): PoSCalculator =
    if (fairPosActivated(height)) FairPoSCalculator
    else NxtPoSCalculator

  def consensusData(
      account: KeyPair,
      height: Int,
      targetBlockDelay: FiniteDuration,
      refBlockBT: Long,
      refBlockTS: Long,
      greatGrandParentTS: Option[Long],
      currentTime: Long
  ): Either[ValidationError, NxtLikeConsensusBlockData] = {
    val bt = pos(height).calculateBaseTarget(targetBlockDelay.toSeconds, height, refBlockBT, refBlockTS, greatGrandParentTS, currentTime)

    checkBaseTargetLimit(bt, height).flatMap(
      _ =>
        blockchain.lastBlock
          .map(_.header.generationSignature.arr)
          .map { gs =>
            val signature = if (vrfActivated(height)) generationVRFSignature(gs, account.privateKey) else generationSignature(gs, account.publicKey)
            NxtLikeConsensusBlockData(bt, ByteStr(signature))
          }
          .toRight(GenericError("No blocks in blockchain"))
    )
  }

  def getValidBlockDelay(height: Int, accountPublicKey: PublicKey, refBlockBT: Long, balance: Long): Either[ValidationError, Long] = {
    val pc = pos(height)

    getHit(height, accountPublicKey)
      .map(pc.calculateDelay(_, refBlockBT, balance))
      .toRight(GenericError("No blocks in blockchain"))
  }

  def validateBlockDelay(height: Int, block: Block, parent: BlockHeader, effectiveBalance: Long): Either[ValidationError, Unit] = {
    getValidBlockDelay(height, block.header.generator, parent.baseTarget, effectiveBalance)
      .map(_ + parent.timestamp)
      .ensureOr { mvt =>
        GenericError(s"Block timestamp ${block.header.timestamp} less than min valid timestamp $mvt")
      }(ts => ts <= block.header.timestamp)
      .map(_ => ())
  }

  def validateGeneratorSignature(height: Int, block: Block): Either[ValidationError, Unit] = {
    val blockGS = block.header.generationSignature.arr

    blockchain.lastBlock
      .toRight(GenericError("No blocks in blockchain"))
      .flatMap {
        case b if vrfActivated(height) =>
          val gs = crypto.verifyVRF(b.header.generationSignature, b.header.generationSignature, block.header.generator).arr
          Either.cond(
            gs sameElements blockGS,
            (),
            GenericError("Generation VRF signatures is not valid")
          )
        case b =>
          val gs = generationSignature(b.header.generationSignature.arr, block.header.generator)
          Either.cond(
            gs sameElements blockGS,
            (),
            GenericError(s"Generation signatures does not match: Expected = ${Base58.encode(gs)}; Found = ${Base58.encode(blockGS)}")
          )
      }
  }

  def checkBaseTargetLimit(baseTarget: Long, height: Int): Either[ValidationError, Unit] = {
    def stopNode(): ValidationError = {
      log.error(
        s"Base target reached maximum value (settings: synchronization.max-base-target=${syncSettings.maxBaseTargetOpt.getOrElse(-1)}). Anti-fork protection."
      )
      log.error("FOR THIS REASON THE NODE WAS STOPPED AUTOMATICALLY")
      forceStopApplication(BaseTargetReachedMaximum)
      GenericError("Base target reached maximum")
    }

    Either.cond(
      // We need to choose some moment with stable baseTarget value in case of loading blockchain from beginning.
      !fairPosActivated(height) || syncSettings.maxBaseTargetOpt.forall(baseTarget < _),
      (),
      stopNode()
    )
  }

  def validateBaseTarget(height: Int, block: Block, parent: BlockHeader, grandParent: Option[BlockHeader]): Either[ValidationError, Unit] = {
    val blockBT = block.header.baseTarget
    val blockTS = block.header.timestamp

    val expectedBT = pos(height).calculateBaseTarget(
      blockchainSettings.genesisSettings.averageBlockDelay.toSeconds,
      height,
      parent.baseTarget,
      parent.timestamp,
      grandParent.map(_.timestamp),
      blockTS
    )

    Either.cond(
      expectedBT == blockBT,
      checkBaseTargetLimit(blockBT, height),
      GenericError(s"declared baseTarget $blockBT does not match calculated baseTarget $expectedBT")
    )
  }

  private def getHit(height: Int, accountPublicKey: PublicKey): Option[BigInt] = {
    val blockForHit =
      if (fairPosActivated(height) && height > 100) blockchain.blockAt(height - 100)
      else blockchain.lastBlock

    blockForHit.map { b =>
      if (!vrfActivated(height))
        hit(generationSignature(b.header.generationSignature.arr, accountPublicKey))
      else
        hit(crypto.verifyVRF(b.header.generationSignature, b.header.generationSignature, b.header.generator).arr)
    }
  }

  private def fairPosActivated(height: Int): Boolean = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.FairPoS.id)
  private def vrfActivated(height: Int): Boolean     = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.BlockV5.id)
}
