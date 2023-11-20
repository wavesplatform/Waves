package com.wavesplatform.state.diffs

import com.wavesplatform.TestValues
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, BlockSnapshot}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lagonaki.mocks.TestBlock.BlockWithSigner
import com.wavesplatform.mining.{MinerImpl, MiningConstraint}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.diffs.BlockDiffer.Result
import com.wavesplatform.state.{Blockchain, Diff, SnapshotBlockchain, StateSnapshot, TxStateSnapshotHashBuilder}
import com.wavesplatform.test.*
import com.wavesplatform.test.node.*
import com.wavesplatform.transaction.TxValidationError.InvalidStateHash
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.utils.Schedulers
import io.netty.channel.group.DefaultChannelGroup
import monix.reactive.Observable

class BlockDifferTest extends FreeSpec with WithDomain {
  private val TransactionFee = 10

  private val signerA, signerB = randomKeyPair()

  private val testChain: Seq[BlockWithSigner] = {
    val master, recipient = randomKeyPair()
    getTwoMinersBlockChain(master, recipient, 9)
  }

  "BlockDiffer" - {
    "enableMicroblocksAfterHeight" - {
      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |5  |10   |A       |10         |10+10=20   |0          |20         |
      |6  |10   |B       |0          |20         |+10        |20+10=30   |
      |7  |10   |A       |10         |20+10=30   |0          |30         |
      |8  |10   |B       |0          |30         |+10        |30+10=40   |
      |9  |10   |A       |10         |30+10=40   |0          |40         | <- 1st check
      |10 |10   |B       |0          |40         |+10        |40+10=50   | <- 2nd check
       */
      "height < enableMicroblocksAfterHeight - a miner should receive 100% of the current block's fee" in {
        assertDiff(testChain.init, 1000) { case (_, s) =>
          s.balance(signerA.toAddress) shouldBe 40
        }

        assertDiff(testChain, 1000) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 50
        }
      }

      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |5  |10   |A       |10         |10+10=20   |0          |20         |
      |6  |10   |B       |0          |20         |+10        |20+10=30   |
      |7  |10   |A       |10         |20+10=30   |0          |30         |
      |8  |10   |B       |0          |30         |+10        |30+10=40   |
      |9  |10   |A       |10         |30+10=40   |0          |40         |
      |-------------------------- Enable NG -----------------------------|
      |10 |10   |B       |0          |40         |+4         |40+4=44    | <- check
       */
      "height = enableMicroblocksAfterHeight - a miner should receive 40% of the current block's fee only" in {
        assertDiff(testChain, 9) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 44
        }
      }

      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |-------------------------- Enable NG -----------------------------|
      |5  |10   |A       |4          |10+4=14    |0          |20         |
      |6  |10   |B       |0          |14         |+4+6=10    |20+10=30   |
      |7  |10   |A       |4+6=10     |14+10=24   |0          |30         |
      |8  |10   |B       |0          |24         |+4+6=10    |30+10=40   |
      |9  |10   |A       |4+6=10     |24+10=34   |0          |40         | <- 1st check
      |10 |10   |B       |0          |34         |+4+6=10    |40+10=50   | <- 2nd check
       */
      "height > enableMicroblocksAfterHeight - a miner should receive 60% of previous block's fee and 40% of the current one" in {
        assertDiff(testChain.init, 4) { case (_, s) =>
          s.balance(signerA.toAddress) shouldBe 34
        }

        assertDiff(testChain, 4) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 50
        }
      }
    }

    "correctly computes state hash" - {
      "genesis block" in {
        val txs = (1 to 10).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 100.waves)) ++
          (1 to 5).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 1.waves))
        withDomain(DomainPresets.TransactionStateSnapshot) { d =>
          val block = createGenesisWithStateHash(txs, txStateSnapshotActivated = true)

          block.header.stateHash shouldBe defined
          BlockDiffer
            .fromBlock(d.blockchain, None, block, None, MiningConstraint.Unlimited, block.header.generationSignature) should beRight
        }

        withDomain(DomainPresets.RideV6) { d =>
          val block = createGenesisWithStateHash(txs, txStateSnapshotActivated = false)

          block.header.stateHash shouldBe None
          BlockDiffer
            .fromBlock(d.blockchain, None, block, None, MiningConstraint.Unlimited, block.header.generationSignature) should beRight
        }
      }

      "arbitrary block/microblock" in
        withDomain(DomainPresets.TransactionStateSnapshot) { d =>
          val genesis = createGenesisWithStateHash(Seq(TxHelpers.genesis(TxHelpers.address(1))), txStateSnapshotActivated = true)
          d.appendBlock(genesis)

          val txs = (1 to 10).map(idx => TxHelpers.transfer(TxHelpers.signer(idx), TxHelpers.address(idx + 1), (100 - idx).waves))

          val blockTs    = txs.map(_.timestamp).max
          val signer     = TxHelpers.signer(2)
          val blockchain = SnapshotBlockchain(d.blockchain, Some(d.settings.blockchainSettings.rewardsSettings.initial))
          val initSnapshot = BlockDiffer
            .createInitialBlockSnapshot(d.blockchain, d.lastBlock.id(), signer.toAddress)
            .explicitGet()
          val initStateHash = TxStateSnapshotHashBuilder.createHashFromSnapshot(initSnapshot, None).createHash(genesis.header.stateHash.get)
          val blockStateHash = TxStateSnapshotHashBuilder
            .computeStateHash(
              txs,
              initStateHash,
              initSnapshot,
              signer,
              d.blockchain.lastBlockTimestamp,
              blockTs,
              isChallenging = false,
              blockchain
            )
            .resultE
            .explicitGet()

          val correctBlock =
            TestBlock.create(blockTs, genesis.id(), txs, signer, version = Block.ProtoBlockVersion, stateHash = Some(blockStateHash))
          BlockDiffer
            .fromBlock(
              blockchain,
              Some(genesis),
              correctBlock.block,
              None,
              MiningConstraint.Unlimited,
              correctBlock.block.header.generationSignature
            ) should beRight

          val incorrectBlock =
            TestBlock
              .create(blockTs, genesis.id(), txs, signer, version = Block.ProtoBlockVersion, stateHash = Some(ByteStr.fill(DigestLength)(1)))
              .block
          BlockDiffer.fromBlock(
            blockchain,
            Some(genesis),
            incorrectBlock,
            None,
            MiningConstraint.Unlimited,
            incorrectBlock.header.generationSignature
          ) shouldBe an[Left[InvalidStateHash, Result]]

          d.appendKeyBlock(signer = signer)
          val correctMicroblock =
            d.createMicroBlock(
              Some(
                TxStateSnapshotHashBuilder
                  .computeStateHash(
                    txs,
                    genesis.header.stateHash.get,
                    StateSnapshot.empty,
                    signer,
                    d.blockchain.lastBlockTimestamp,
                    blockTs,
                    isChallenging = false,
                    blockchain
                  )
                  .resultE
                  .explicitGet()
              )
            )(
              txs*
            )
          BlockDiffer.fromMicroBlock(
            blockchain,
            blockchain.lastBlockTimestamp,
            genesis.header.stateHash.get,
            correctMicroblock,
            None,
            MiningConstraint.Unlimited
          ) should beRight

          val incorrectMicroblock = d.createMicroBlock(Some(ByteStr.fill(DigestLength)(1)))(txs*)
          BlockDiffer.fromMicroBlock(
            blockchain,
            blockchain.lastBlockTimestamp,
            genesis.header.stateHash.get,
            incorrectMicroblock,
            None,
            MiningConstraint.Unlimited
          ) shouldBe an[Left[InvalidStateHash, Result]]
        }
    }

    "result of txs validation should be equal the result of snapshot apply" in {
      val sender = TxHelpers.signer(1)
      withDomain(DomainPresets.TransactionStateSnapshot, AddrWithBalance.enoughBalances(sender)) { d =>
        (1 to 5).map { idx =>
          val (refBlock, refSnapshot, carry, _, refStateHash, _) = d.liquidState.get.snapshotOf(d.lastBlock.id()).get
          val refBlockchain = SnapshotBlockchain(
            d.rocksDBWriter,
            refSnapshot,
            refBlock,
            d.liquidState.get.hitSource,
            carry,
            d.blockchain.computeNextReward,
            Some(refStateHash)
          )

          val block = d.createBlock(Block.ProtoBlockVersion, Seq(TxHelpers.transfer(sender, amount = idx.waves, fee = TestValues.fee * idx)))
          val hs    = d.posSelector.validateGenerationSignature(block).explicitGet()
          val txValidationResult = BlockDiffer.fromBlock(refBlockchain, Some(refBlock), block, None, MiningConstraint.Unlimited, hs)

          val txInfo        = txValidationResult.explicitGet().snapshot.transactions.head._2
          val blockSnapshot = BlockSnapshot(block.id(), Seq(txInfo.snapshot -> txInfo.status))

          val snapshotApplyResult = BlockDiffer.fromBlock(refBlockchain, Some(refBlock), block, Some(blockSnapshot), MiningConstraint.Unlimited, hs)

          // TODO: remove after NODE-2610 fix
          def clearAffected(r: Result): Result = {
            r.copy(
              snapshot = r.snapshot.copy(transactions = r.snapshot.transactions.map { case (id, info) => id -> info.copy(affected = Set.empty) }),
              keyBlockSnapshot = r.keyBlockSnapshot.copy(transactions = r.keyBlockSnapshot.transactions.map { case (id, info) =>
                id -> info.copy(affected = Set.empty)
              })
            )

          }

          val snapshotApplyResultWithoutAffected = snapshotApplyResult.map(clearAffected)
          val txValidationResultWithoutAffected  = txValidationResult.map(clearAffected)

          snapshotApplyResultWithoutAffected shouldBe txValidationResultWithoutAffected
        }
      }
    }

    "should be possible to append key block that references non-last microblock (NODE-1172)" in {
      val sender   = TxHelpers.signer(1)
      val minerAcc = TxHelpers.signer(2)
      val settings = DomainPresets.TransactionStateSnapshot
      withDomain(
        settings.copy(minerSettings = settings.minerSettings.copy(quorum = 0)),
        AddrWithBalance.enoughBalances(sender, minerAcc)
      ) { d =>
        d.appendBlock()
        val time = TestTime()

        val miner = new MinerImpl(
          new DefaultChannelGroup("", null),
          d.blockchain,
          d.settings,
          time,
          d.utxPool,
          d.wallet,
          d.posSelector,
          Schedulers.singleThread("miner"),
          Schedulers.singleThread("appender"),
          Observable.empty
        )

        val refId = d.appendMicroBlock(TxHelpers.transfer(sender, amount = 1))
        Thread.sleep(d.settings.minerSettings.minMicroBlockAge.toMillis)
        d.appendMicroBlock(TxHelpers.transfer(sender, amount = 2))

        time.setTime(System.currentTimeMillis() + 2 * d.settings.blockchainSettings.genesisSettings.averageBlockDelay.toMillis)
        val (block, _) = miner.forgeBlock(minerAcc).explicitGet()

        block.header.reference shouldBe refId

        d.appendBlockE(block) should beRight
      }
    }
  }

  private def assertDiff(blocks: Seq[BlockWithSigner], ngAtHeight: Int)(assertion: (Diff, Blockchain) => Unit): Unit = {
    val fs = FunctionalitySettings(
      featureCheckBlocksPeriod = ngAtHeight / 2,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = Map[Short, Int]((2, ngAtHeight)),
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue
    )
    assertNgDiffState(blocks.init, blocks.last, fs)(assertion)
  }

  private def getTwoMinersBlockChain(from: KeyPair, to: KeyPair, numPayments: Int): Seq[BlockWithSigner] = {
    val genesisTx            = TxHelpers.genesis(from.toAddress, Long.MaxValue - 1)
    val features: Seq[Short] = Seq[Short](2)

    val paymentTxs = (1 to numPayments).map { i =>
      TxHelpers.transfer(from, to.toAddress, 10000, fee = TransactionFee, version = TxVersion.V1)
    }

    (genesisTx +: paymentTxs).zipWithIndex.map { case (x, i) =>
      val signer = if (i % 2 == 0) signerA else signerB
      TestBlock.create(signer, Seq(x), features)
    }
  }
}
