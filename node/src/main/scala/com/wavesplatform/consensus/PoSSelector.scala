package com.wavesplatform.consensus

import cats.syntax.either._
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

    checkBaseTargetLimit(bt, height).flatMap { _ =>
      val nextHeight = height + 1

      val generationSignature =
        if (vrfActivated(nextHeight))
          hitSourceForHeight(nextHeight)
            .map(hitSource => PoSCalculator.generationVRFSignature(hitSource.arr, account.privateKey))
        else
          blockchain
            .blockInfo(height)
            .map(blockInfo => PoSCalculator.generationSignature(blockInfo.header.generationSignature.arr, account.publicKey))
            .toRight(GenericError(s"Couldn't find block info at height: $height"))

      generationSignature.map(genSig => NxtLikeConsensusBlockData(bt, ByteStr(genSig)))
    }
  }

  def getValidBlockDelay(height: Int, accountPublicKey: PublicKey, refBlockBT: Long, balance: Long): Either[ValidationError, Long] =
    hitSourceForHeight(height)
      .map(hitSource => PoSCalculator.hit(PoSCalculator.generationSignature(hitSource.arr, accountPublicKey)))
      .map(pos(height).calculateDelay(_, refBlockBT, balance))

  def validateBlockDelay(height: Int, block: Block, parent: BlockHeader, effectiveBalance: Long): Either[ValidationError, Unit] = {
    getValidBlockDelay(height, block.header.generator, parent.baseTarget, effectiveBalance)
      .map(_ + parent.timestamp)
      .ensureOr { mvt =>
        GenericError(s"Block timestamp ${block.header.timestamp} less than min valid timestamp $mvt")
      }(ts => ts <= block.header.timestamp)
      .map(_ => ())
  }

  def validateGenerationSignature(block: Block): Either[ValidationError, ByteStr] = {
    val nextHeight = blockchain.height + 1
    val genSig     = block.header.generationSignature
    val generator  = block.header.generator

    if (vrfActivated(nextHeight))
      hitSourceForHeight(nextHeight)
        .flatMap(crypto.verifyVRF(genSig, _, generator))
    else
      blockchain.lastBlock
        .toRight(GenericError("No blocks in blockchain"))
        .map(b => ByteStr(PoSCalculator.generationSignature(b.header.generationSignature.arr, generator)))
        .ensureOr { expectedGenSig =>
          GenericError(s"Generation signatures does not match: Expected = ${Base58.encode(expectedGenSig)}; Found = ${Base58.encode(genSig)}")
        } { expectedGenSig =>
          genSig.arr sameElements expectedGenSig.arr
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

  private def hitSourceForHeight(height: Int): Either[ValidationError, ByteStr] = {
    val hitHeight = if (fairPosActivated(height) && height > 100) height - 100 else blockchain.height
    blockchain
      .hitSourceAtHeight(hitHeight)
      .toRight(GenericError(s"Couldn't find hit source at height: $hitHeight"))
  }

  private def fairPosActivated(height: Int): Boolean = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.FairPoS.id)
  private def vrfActivated(height: Int): Boolean     = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.BlockV5.id)
}
