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

    checkBaseTargetLimit(bt, height).flatMap { _ =>
      blockchain
        .blockProofsAtHeight(height)
        .map(parentProofs => NxtLikeConsensusBlockData(bt, blockProofs(height, parentProofs, account)))
    }
  }

  def getValidBlockDelay(height: Int, accountPublicKey: PublicKey, refBlockBT: Long, balance: Long): Either[ValidationError, Long] = {
    val pc = pos(height)

    getHit(height, accountPublicKey)
      .map(pc.calculateDelay(_, refBlockBT, balance))
  }

  def validateTimestamp(height: Int, proofs: ByteStr, timestamp: Long, parent: BlockHeader, effectiveBalance: Long): Either[ValidationError, Unit] =
    for {
      _ <- Either.cond(proofs.size == ProofsSize, (), GenericError("Illegal size of block generation proofs"))
      valid = pos(height).calculateDelay(hit(proofs.arr), parent.baseTarget, effectiveBalance) + parent.timestamp
      _ <- if (valid <= timestamp) Right(()) else Left(GenericError(s"Block timestamp $timestamp less than min valid timestamp $valid"))
    } yield ()

  def validateBlockDelay(height: Int, block: Block, parent: BlockHeader, effectiveBalance: Long): Either[ValidationError, Unit] = {
    getValidBlockDelay(height, block.header.generator, parent.baseTarget, effectiveBalance)
      .map(_ + parent.timestamp)
      .ensureOr { mvt =>
        GenericError(s"Block timestamp ${block.header.timestamp} less than min valid timestamp $mvt")
      }(ts => ts <= block.header.timestamp)
      .map(_ => ())
  }

  def validateGeneratorSignature(height: Int, block: Block): Either[ValidationError, ByteStr] = {
    val blockGS        = block.header.generationSignature.arr
    val blockGenerator = block.header.generator

    blockchain.lastBlock
      .toRight(GenericError("No blocks in blockchain"))
      .flatMap {
        case _ if vrfActivated(height) => // todo: (NODE-1927) always last block or by height?
          for {
            proofs <- blockchain.blockProofsAtHeight(blockchain.height)
            // proofs <- blockchain.blockHitAtHeight(height)
            vrf <- crypto.verifyVRF(blockGS, proofs, blockGenerator)
          } yield vrf
        case b =>
          val gs = generationSignature(b.header.generationSignature.arr, blockGenerator)
          Either.cond(
            gs sameElements blockGS,
            ByteStr(gs),
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

  private def getHit(height: Int, accountPublicKey: PublicKey): Either[ValidationError, BigInt] = {
    val message =
      if (fairPosActivated(height) && height > 100) blockchain.blockProofsAtHeight(height - 1)
      else blockchain.blockProofsAtHeight(blockchain.height)

    message.map(msg => if (vrfActivated(height)) msg.arr else generationSignature(msg, accountPublicKey)).map(msg => hit(msg))
  }

  private def blockProofs(height: Int, parentProofs: ByteStr, account: KeyPair): ByteStr =
    if (vrfActivated(height))
      generationVRFSignature(parentProofs.arr, account.privateKey)
    else
      generationSignature(parentProofs.arr, account.publicKey)

  private def fairPosActivated(height: Int): Boolean = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.FairPoS.id)
  private def vrfActivated(height: Int): Boolean     = blockchain.activatedFeaturesAt(height).contains(BlockchainFeatures.BlockV5.id)
}
