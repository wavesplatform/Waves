package com.wavesplatform.state

import com.wavesplatform.block.{Block, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.network.{BlockSnapshotResponse, ExtensionBlocks, InvalidBlockStorage, PeerDatabase}
import com.wavesplatform.protobuf.PBSnapshots
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.appender.{BlockAppender, ExtensionAppender}
import com.wavesplatform.state.diffs.BlockDiffer
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.TxValidationError.InvalidStateHash
import io.netty.channel.embedded.EmbeddedChannel
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

import scala.collection.immutable.VectorMap

class LightNodeTest extends PropSpec with WithDomain {

  val settings: WavesSettings   = DomainPresets.TransactionStateSnapshot.copy(enableLightMode = true)
  val invalidStateHash: ByteStr = ByteStr.fill(32)(1)

  property("NODE-1148. Light node shouldn't apply block when its state hash differs from snapshot state hash") {
    val sender    = TxHelpers.signer(1)
    val recipient = TxHelpers.address(2)
    withDomain(settings.configure(_.copy(lightNodeBlockFieldsAbsenceInterval = 0)), AddrWithBalance.enoughBalances(sender)) { d =>
      val prevBlock    = d.lastBlock
      val txs          = Seq(TxHelpers.transfer(sender, recipient, amount = 10.waves), TxHelpers.transfer(sender, recipient, amount = 100.waves))
      val validBlock   = d.createBlock(Block.ProtoBlockVersion, txs)
      val invalidBlock = d.createBlock(Block.ProtoBlockVersion, txs, stateHash = Some(Some(invalidStateHash)))
      val txSnapshots  = getTxSnapshots(d, validBlock)

      d.appendBlockE(
        invalidBlock,
        Some(BlockSnapshot(invalidBlock.id(), txSnapshots))
      ) shouldBe Left(InvalidStateHash(Some(invalidStateHash)))
      d.lastBlock shouldBe prevBlock

      d.appendBlockE(
        validBlock,
        Some(BlockSnapshot(validBlock.id(), txSnapshots))
      ) should beRight
      d.lastBlock shouldBe validBlock
    }
  }

  property("NODE-1149. Light node may apply block with invalid state hash if snapshot state hash is equal to block state hash") {
    val sender          = TxHelpers.signer(1)
    val recipient       = TxHelpers.address(2)
    val txs             = Seq(TxHelpers.transfer(sender, recipient, amount = 10.waves), TxHelpers.transfer(sender, recipient, amount = 100.waves))
    val invalidBlockTxs = Seq(TxHelpers.transfer(sender, recipient, amount = 20.waves), TxHelpers.transfer(sender, recipient, amount = 200.waves))

    withDomain(settings, AddrWithBalance.enoughBalances(sender)) { d =>
      val validBlockWithOtherTxs = d.createBlock(Block.ProtoBlockVersion, txs)

      val invalidBlock = d.createBlock(Block.ProtoBlockVersion, invalidBlockTxs, stateHash = Some(validBlockWithOtherTxs.header.stateHash))

      val txSnapshots = getTxSnapshots(d, validBlockWithOtherTxs)

      d.appendBlockE(invalidBlock, Some(BlockSnapshot(invalidBlock.id(), txSnapshots))) should beRight
      d.lastBlock shouldBe invalidBlock
    }
  }

  property(" NODE-1143. Rollback returns discarded block snapshots only for light node") {
    val sender    = TxHelpers.signer(1)
    val recipient = TxHelpers.address(2)

    Seq(true -> None, false -> Some(List.empty[BlockSnapshot])).foreach { case (isLightMode, maybeExpectedSnapshots) =>
      withDomain(DomainPresets.TransactionStateSnapshot.copy(enableLightMode = isLightMode), AddrWithBalance.enoughBalances(sender)) { d =>
        val genesisSignature = d.lastBlockId

        def newBlocks(count: Int): List[BlockSnapshot] = {
          if (count == 0) {
            Nil
          } else {
            val txs =
              Seq(
                TxHelpers.transfer(sender, recipient, amount = (count + 1).waves),
                TxHelpers.transfer(sender, recipient, amount = (count + 2).waves)
              )
            val block       = d.createBlock(Block.ProtoBlockVersion, txs)
            val txSnapshots = getTxSnapshots(d, block).map { case (snapshot, status) => snapshot.copy(transactions = VectorMap.empty) -> status }
            d.appendBlock(block)
            BlockSnapshot(block.id(), txSnapshots) :: newBlocks(count - 1)
          }
        }

        val blockSnapshots  = newBlocks(10)
        val discardedBlocks = d.rollbackTo(genesisSignature)
        discardedBlocks.head._1.header.reference shouldBe genesisSignature
        discardedBlocks.flatMap(_._3).toList shouldBe maybeExpectedSnapshots.getOrElse(blockSnapshots)
        discardedBlocks.foreach { case (block, _, snapshot) =>
          d.appendBlockE(block, snapshot) should beRight
        }
      }
    }
  }

  property("NODE-1165, NODE-1166. Full and light nodes should correctly switch to branch with better score") {
    val sender    = TxHelpers.signer(1)
    val recipient = TxHelpers.address(2)

    Seq(true, false).foreach { isLightMode =>
      withDomain(
        DomainPresets.TransactionStateSnapshot.copy(enableLightMode = isLightMode),
        AddrWithBalance.enoughBalances(sender, TxHelpers.defaultSigner)
      ) { d =>
        val chainSize = 3
        val genesisId = d.lastBlockId
        val betterBlocks = (1 to chainSize).map { idx =>
          val txs =
            Seq(TxHelpers.transfer(sender, recipient, amount = (idx + 10).waves), TxHelpers.transfer(sender, recipient, amount = (idx + 11).waves))
          val block       = d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true)
          val txSnapshots = if (isLightMode) Some(getTxSnapshots(d, block)) else None
          d.appendBlock(block)
          block -> txSnapshots
        }
        val expectedStateHash = d.lastBlock.header.stateHash
        d.rollbackTo(genesisId)

        (1 to chainSize).foreach { idx =>
          val txs = Seq(TxHelpers.transfer(sender, recipient, amount = idx.waves), TxHelpers.transfer(sender, recipient, (idx + 1).waves))
          d.appendBlock(txs*)
        }
        val currentScore = d.blockchain.score

        val extensionBlocks = ExtensionBlocks(
          currentScore + 1,
          betterBlocks.map(_._1),
          betterBlocks.collect { case (b, Some(snapshots)) =>
            b.id() -> BlockSnapshotResponse(b.id(), snapshots.map { case (s, m) => PBSnapshots.toProtobuf(s, m) })
          }.toMap
        )

        val appender =
          ExtensionAppender(
            d.blockchain,
            d.utxPool,
            d.posSelector,
            TestTime(extensionBlocks.blocks.last.header.timestamp),
            InvalidBlockStorage.NoOp,
            PeerDatabase.NoOp,
            Scheduler.global
          )(
            new EmbeddedChannel(),
            _
          )

        appender(extensionBlocks).runSyncUnsafe() should beRight
        d.lastBlock.header.stateHash shouldBe expectedStateHash
        d.blockchain.height shouldBe chainSize + 1
        d.blocksApi.blocksRange(2, d.blockchain.height).toListL.runSyncUnsafe().map(_._1.header) shouldBe betterBlocks.map(_._1.header)
      }
    }
  }

  property("NODE-1168. Light node should correctly apply challenging block") {
    val sender           = TxHelpers.signer(1)
    val recipient        = TxHelpers.address(2)
    val challengingMiner = TxHelpers.signer(3)

    withDomain(settings, AddrWithBalance.enoughBalances(challengingMiner, TxHelpers.defaultSigner, sender)) { d =>
      val txs              = Seq(TxHelpers.transfer(sender, recipient, amount = 1.waves), TxHelpers.transfer(sender, recipient, amount = 2.waves))
      val invalidBlock     = d.createBlock(Block.ProtoBlockVersion, txs, strictTime = true, stateHash = Some(Some(invalidStateHash)))
      val challengingBlock = d.createChallengingBlock(challengingMiner, invalidBlock, strictTime = true)
      val txSnapshots      = getTxSnapshots(d, challengingBlock)

      val appender = BlockAppender(d.blockchainUpdater, TestTime(challengingBlock.header.timestamp), d.utxPool, d.posSelector, Scheduler.global) _

      val sr = BlockSnapshotResponse(challengingBlock.id(), txSnapshots.map { case (s, m) => PBSnapshots.toProtobuf(s, m) })
      appender(challengingBlock, Some(sr)).runSyncUnsafe() shouldBe Right(
        Applied(Seq.empty, d.blockchain.score)
      )
      d.lastBlock shouldBe challengingBlock
    }
  }

  private def getTxSnapshots(d: Domain, block: Block): Seq[(StateSnapshot, TxMeta.Status)] = {
    val (refBlock, refSnapshot, carry, _, prevStateHash, _) = d.liquidState.get.snapshotOf(block.header.reference).get

    val hs = d.posSelector.validateGenerationSignature(block).explicitGet()

    val referencedBlockchain = SnapshotBlockchain(
      d.rocksDBWriter,
      refSnapshot,
      refBlock,
      d.liquidState.get.hitSource,
      carry,
      Some(d.settings.blockchainSettings.rewardsSettings.initial),
      Some(prevStateHash)
    )

    val snapshot =
      BlockDiffer
        .fromBlock(referencedBlockchain, Some(refBlock), block, None, MiningConstraint.Unlimited, hs, None)
        .explicitGet()
        .snapshot

    snapshot.transactions.values.toSeq.map(txInfo => txInfo.snapshot -> txInfo.status)
  }
}
