package com.wavesplatform.history

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.diffs.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.transfer.*
import org.scalacheck.Gen

class BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest extends PropSpec with DomainScenarioDrivenPropertyCheck {

  import BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest.*

  type Setup = (GenesisTransaction, TransferTransaction, TransferTransaction, TransferTransaction)

  property("resulting miner balance should not depend on tx distribution among blocks and microblocks") {
    forAll(g(100, 5)) { case (gen, rest) =>
      val finalMinerBalances = rest.map { case (bmb: BlockAndMicroblockSequence, last: Block) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          d.blockchainUpdater.processBlock(gen) should beRight
          bmb.foreach { case (b, mbs) =>
            d.blockchainUpdater.processBlock(b) should beRight
            mbs.foreach(mb => d.blockchainUpdater.processMicroBlock(mb, None) should beRight)
          }
          d.blockchainUpdater.processBlock(last)
          d.balance(last.header.generator.toAddress)
        }
      }
      finalMinerBalances.toSet.size shouldBe 1
    }
  }

  property("Miner fee from microblock [Genesis] <- [Empty] <~ (Micro with tx) <- [Empty]") {
    val preconditionsAndPayments: Gen[(KeyPair, GenesisTransaction, TransferTransaction, Int)] = for {
      master <- accountGen
      miner  <- accountGen
      ts     <- positiveIntGen
      fee    <- smallFeeGen
      amt    <- smallFeeGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      payment: TransferTransaction = createWavesTransfer(master, master.toAddress, amt, fee, ts).explicitGet()
    } yield (miner, genesis, payment, ts)
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (miner, genesis, payment, ts)) =>
      val genBlock       = buildBlockOfTxs(randomSig, Seq(genesis))
      val (base, micros) = chainBaseAndMicro(genBlock.id(), Seq.empty, Seq(Seq(payment)), miner, 3, ts)
      val emptyBlock     = customBuildBlockOfTxs(micros.last.totalResBlockSig, Seq.empty, miner, 3, ts)
      domain.blockchainUpdater.processBlock(genBlock) should beRight
      domain.blockchainUpdater.processBlock(base) should beRight
      domain.blockchainUpdater.processMicroBlock(micros.head, None) should beRight
      domain.blockchainUpdater.processBlock(emptyBlock) should beRight

      domain.balance(miner.toAddress) shouldBe payment.fee.value
      domain.balance(genesis.recipient) shouldBe (genesis.amount.value - payment.fee.value)
    }
  }

  property("Microblock tx sequence") {
    val txCount         = 10
    val microBlockCount = 10
    val preconditionsAndPayments: Gen[(KeyPair, GenesisTransaction, Seq[Seq[TransferTransaction]], Int)] =
      for {
        master <- accountGen
        miner  <- accountGen
        ts     <- positiveIntGen
        fee    <- smallFeeGen
        amt    <- smallFeeGen
        genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
        microBlockTxs = (1 to txCount * microBlockCount)
          .map(step => createWavesTransfer(master, master.toAddress, amt, fee, ts + step).explicitGet())
          .grouped(microBlockCount)
          .toSeq
      } yield (miner, genesis, microBlockTxs, ts)
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (miner, genesis, microBlockTxs, ts)) =>
      val genBlock       = buildBlockOfTxs(randomSig, Seq(genesis))
      val (base, micros) = chainBaseAndMicro(genBlock.id(), Seq.empty, microBlockTxs, miner, 3, ts)
      val emptyBlock     = customBuildBlockOfTxs(micros.last.totalResBlockSig, Seq.empty, miner, 3, ts)
      domain.blockchainUpdater.processBlock(genBlock) should beRight
      domain.blockchainUpdater.processBlock(base) should beRight
      micros.foreach(domain.blockchainUpdater.processMicroBlock(_, None) should beRight)
      domain.blockchainUpdater.processBlock(emptyBlock) should beRight

      domain.rocksDBWriter.lastBlock.get.transactionData shouldBe microBlockTxs.flatten
    }
  }

  def randomPayment(accs: Seq[KeyPair], ts: Long): Gen[TransferTransaction] =
    for {
      from <- Gen.oneOf(accs)
      to   <- Gen.oneOf(accs)
      fee  <- smallFeeGen
      amt  <- smallFeeGen
    } yield createWavesTransfer(from, to.toAddress, amt, fee, ts).explicitGet()

  def randomPayments(accs: Seq[KeyPair], ts: Long, amt: Int): Gen[Seq[TransferTransaction]] =
    if (amt == 0)
      Gen.const(Seq.empty)
    else
      for {
        h <- randomPayment(accs, ts)
        t <- randomPayments(accs, ts + 1, amt - 1)
      } yield h +: t

  val TOTAL_WAVES = ENOUGH_AMT

  def accsAndGenesis(): Gen[(Seq[KeyPair], KeyPair, Block, Int)] =
    for {
      alice   <- accountGen
      bob     <- accountGen
      charlie <- accountGen
      dave    <- accountGen
      miner   <- accountGen
      ts      <- positiveIntGen
      genesis1: GenesisTransaction = GenesisTransaction.create(alice.toAddress, TOTAL_WAVES / 4, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(bob.toAddress, TOTAL_WAVES / 4, ts + 1).explicitGet()
      genesis3: GenesisTransaction = GenesisTransaction.create(charlie.toAddress, TOTAL_WAVES / 4, ts + 2).explicitGet()
      genesis4: GenesisTransaction = GenesisTransaction.create(dave.toAddress, TOTAL_WAVES / 4, ts + 4).explicitGet()
    } yield (
      Seq(alice, bob, charlie, dave),
      miner,
      customBuildBlockOfTxs(randomSig, Seq(genesis1, genesis2, genesis3, genesis4), defaultSigner, 1, ts),
      ts
    )

  def g(totalTxs: Int, totalScenarios: Int): Gen[(Block, Seq[(BlockAndMicroblockSequence, Block)])] =
    for {
      (accs, miner, genesis, ts)            <- accsAndGenesis()
      payments: Seq[TransferTransaction]    <- randomPayments(accs, ts, totalTxs)
      intSeqs: Seq[BlockAndMicroblockSizes] <- randomSequences(totalTxs, totalScenarios)
    } yield {
      val version = 3: Byte
      val blocksAndMicros = intSeqs.map { intSeq =>
        val blockAndMicroblockSequence = r(payments, intSeq, genesis.id(), miner, version, ts)
        val ref                        = bestRef(blockAndMicroblockSequence.last)
        val lastBlock                  = customBuildBlockOfTxs(ref, Seq.empty, miner, version, ts)
        (blockAndMicroblockSequence, lastBlock)
      }
      (genesis, blocksAndMicros)
    }
}

object BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest {

  def genSizes(total: Int): Gen[Seq[Int]] =
    for {
      h <- Gen.choose(1, total)
      t <- if (h < total) genSizes(total - h) else Gen.const(Seq.empty)
    } yield h +: t

  def genSplitSizes(total: Int): Gen[(Int, Seq[Int])] = genSizes(total).map(s => s.head -> s.tail)

  type BlockAndMicroblockSize     = (Int, Seq[Int])
  type BlockAndMicroblockSizes    = Seq[BlockAndMicroblockSize]
  type BlockAndMicroblocks        = (Block, Seq[MicroBlockWithTotalId])
  type BlockAndMicroblockSequence = Seq[BlockAndMicroblocks]

  def randomSizeSequence(total: Int): Gen[BlockAndMicroblockSizes] =
    for {
      totalStep <- Gen.choose(1, Math.min(Math.min(total / 3 + 2, total), 250))
      h         <- genSplitSizes(totalStep)
      t         <- if (totalStep < total) randomSizeSequence(total - totalStep) else Gen.const(Seq.empty)
    } yield h +: t

  def randomSequences(total: Int, sequences: Int): Gen[Seq[BlockAndMicroblockSizes]] =
    if (sequences == 0)
      Gen.const(Seq.empty)
    else
      for {
        h <- randomSizeSequence(total)
        t <- randomSequences(total, sequences - 1)
      } yield h +: t

  def take(txs: Seq[Transaction], sizes: BlockAndMicroblockSize): ((Seq[Transaction], Seq[Seq[Transaction]]), Seq[Transaction]) = {
    val (blockAmt, microsAmts) = sizes
    val (blockTxs, rest)       = txs.splitAt(blockAmt)
    val (reversedMicroblockTxs, res) = microsAmts.foldLeft((Seq.empty[Seq[Transaction]], rest)) { case ((acc, pool), amt) =>
      val (step, next) = pool.splitAt(amt)
      (step +: acc, next)
    }
    ((blockTxs, reversedMicroblockTxs.reverse), res)
  }

  def stepR(
      txs: Seq[Transaction],
      sizes: BlockAndMicroblockSize,
      prev: ByteStr,
      signer: KeyPair,
      version: TxVersion,
      timestamp: Long
  ): (BlockAndMicroblocks, Seq[Transaction]) = {
    val ((blockTxs, microblockTxs), rest) = take(txs, sizes)
    (chainBaseAndMicro(prev, blockTxs, microblockTxs, signer, version, timestamp), rest)
  }

  def bestRef(r: BlockAndMicroblocks): ByteStr = r._2.lastOption match {
    case Some(mb) => mb.totalBlockId
    case None     => r._1.id()
  }

  def r(
      txs: Seq[Transaction],
      sizes: BlockAndMicroblockSizes,
      initial: ByteStr,
      signer: KeyPair,
      version: TxVersion,
      timestamp: Long
  ): BlockAndMicroblockSequence = {
    sizes
      .foldLeft((Seq.empty[BlockAndMicroblocks], txs)) { case ((acc, rest), s) =>
        val prev         = acc.headOption.map(bestRef).getOrElse(initial)
        val (step, next) = stepR(rest, s, prev, signer, version, timestamp)
        (step +: acc, next)
      }
      ._1
      .reverse
  }
}
