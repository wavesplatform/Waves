package com.wavesplatform.consensus

import cats.syntax.either._
import com.wavesplatform.account.KeyPair
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

  protected def posCalculator(height: Int): PoSCalculator =
    if (fairPosActivated(height))
      if (vrfActivated(height)) FairPoSCalculator.V2
      else FairPoSCalculator.V1
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
    val bt = posCalculator(height).calculateBaseTarget(targetBlockDelay.toSeconds, height, refBlockBT, refBlockTS, greatGrandParentTS, currentTime)

    checkBaseTargetLimit(bt, height).flatMap(
      _ =>
        if (vrfActivated(height + 1))
          getHitSource(height)
            .map(hs => NxtLikeConsensusBlockData(bt, crypto.signVRF(account.privateKey, hs.arr)))
        else
          blockchain
            .blockHeader(height)
            .map(_.header.generationSignature)
            .map(gs => NxtLikeConsensusBlockData(bt, ByteStr(generationSignature(gs, account.publicKey))))
            .toRight(GenericError("No blocks in blockchain"))
    )
  }

  def getValidBlockDelay(height: Int, account: KeyPair, refBlockBT: Long, balance: Long): Either[ValidationError, Long] = {
    val pc = posCalculator(height)

    getHit(height, account)
      .map(pc.calculateDelay(_, refBlockBT, balance))
  }

  def validateBlockDelay(parentHeight: Int, header: BlockHeader, parent: BlockHeader, effectiveBalance: Long): Either[ValidationError, Unit] = {
    for {
      parentHitSource <- getHitSource(parentHeight)
      gs <- if (vrfActivated(parentHeight + 1)) {
        crypto.verifyVRF(header.generationSignature, parentHitSource.arr, header.generator).map(_.arr)
      } else {
        generationSignature(parentHitSource, header.generator).asRight[ValidationError]
      }
      ts = posCalculator(parentHeight).calculateDelay(hit(gs), parent.baseTarget, effectiveBalance) + parent.timestamp
      _ <- Either.cond(
        ts <= header.timestamp,
        (),
        GenericError(s"Block timestamp ${header.timestamp} less than min valid timestamp $ts")
      )
    } yield ()
  }

  def validateGenerationSignature(block: Block): Either[ValidationError, ByteStr] = {
    val blockGenSig = block.header.generationSignature
    val height      = blockchain.height

    if (vrfActivated(height + 1)) {
      getHitSource(height).flatMap(hs => crypto.verifyVRF(blockGenSig, hs.arr, block.header.generator))
    } else {
      blockchain.lastBlockHeader
        .toRight(GenericError("No blocks in blockchain"))
        .map(b => generationSignature(b.header.generationSignature, block.header.generator))
        .ensureOr { expectedGenSig =>
          GenericError(s"Generation signatures does not match: Expected = ${Base58.encode(expectedGenSig)}; Found = $blockGenSig")
        } { expectedGenSig =>
          blockGenSig.arr sameElements expectedGenSig
        }
        .map(_ => block.header.generationSignature)
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

    val expectedBT = posCalculator(height).calculateBaseTarget(
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

  private def getHitSource(height: Int): Either[ValidationError, ByteStr] = {
    val hitSource = if (fairPosActivated(height) && height > 100) blockchain.hitSource(height - 100) else blockchain.hitSource(height)
    hitSource.toRight(GenericError(s"Couldn't find hit source for height: $height"))
  }

  private def getHit(height: Int, account: KeyPair): Either[ValidationError, BigInt] =
    for {
      hitSource <- getHitSource(height)
      gs <- if (vrfActivated(height + 1)) {
        val vrfProof = crypto.signVRF(account.privateKey, hitSource.arr)
        crypto.verifyVRF(vrfProof, hitSource.arr, account.publicKey).map(_.arr)
      } else {
        generationSignature(hitSource, account.publicKey).asRight[ValidationError]
      }
    } yield hit(gs)

  private def fairPosActivated(height: Int): Boolean = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.FairPoS.id)
  private def vrfActivated(height: Int): Boolean     = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.BlockV5.id)
}
