package com.wavesplatform.history

import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, MicroBlock}
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

class BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest extends PropSpec
  with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen with NoShrink {

  import BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest._

  type Setup = (GenesisTransaction, TransferTransaction, TransferTransaction, TransferTransaction)

  property("resulting miner balance should not depend on tx distribution among blocks and microblocks") {
    forAll(g(100, 5)) { case (gen, rest) =>
      val finalMinerBalances = rest.map { case (a@(bmb: BlockAndMicroblockSequence, last: Block)) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          d.blockchainUpdater.processBlock(gen).explicitGet()
          bmb.foreach { case (b, mbs) =>
            d.blockchainUpdater.processBlock(b).explicitGet()
            mbs.foreach(mb => d.blockchainUpdater.processMicroBlock(mb).explicitGet())
          }
          d.blockchainUpdater.processBlock(last)
          d.stateReader.portfolio(last.signerData.generator.toAddress).balance
        }
      }
      finalMinerBalances.toSet.size shouldBe 1
    }
  }

  property("Miner fee from microblock [Genesis] <- [Empty] <~ (Micro with tx) <- [Empty]") {
    val preconditionsAndPayments: Gen[(PrivateKeyAccount, GenesisTransaction, TransferTransaction, Int)] = for {
      master <- accountGen
      miner <- accountGen
      ts <- positiveIntGen
      fee <- smallFeeGen
      amt <- smallFeeGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      payment: TransferTransaction = createWavesTransfer(master, master, amt, fee, ts).explicitGet()
    } yield (miner, genesis, payment, ts)
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (miner, genesis, payment, ts)) =>
      val genBlock = buildBlockOfTxs(randomSig, Seq(genesis))
      val (base, micros) = chainBaseAndMicro(genBlock.uniqueId, Seq.empty, Seq(Seq(payment)), miner, 3, ts)
      val emptyBlock = customBuildBlockOfTxs(micros.last.totalResBlockSig, Seq.empty, miner, 3, ts)
      domain.blockchainUpdater.processBlock(genBlock).explicitGet()
      domain.blockchainUpdater.processBlock(base).explicitGet()
      domain.blockchainUpdater.processMicroBlock(micros.head).explicitGet()
      domain.blockchainUpdater.processBlock(emptyBlock).explicitGet()

      domain.stateReader.portfolio(miner).balance shouldBe payment.fee
      domain.stateReader.portfolio(genesis.recipient).balance shouldBe (genesis.amount - payment.fee)
    }
  }

  def randomPayment(accs: Seq[PrivateKeyAccount], ts: Long): Gen[TransferTransaction] = for {
    from <- Gen.oneOf(accs)
    to <- Gen.oneOf(accs)
    fee <- smallFeeGen
    amt <- smallFeeGen
  } yield createWavesTransfer(from, to, amt, fee, ts).explicitGet()

  def randomPayments(accs: Seq[PrivateKeyAccount], ts: Long, amt: Int): Gen[Seq[TransferTransaction]] =
    if (amt == 0)
      Gen.const(Seq.empty)
    else for {
      h <- randomPayment(accs, ts)
      t <- randomPayments(accs, ts + 1, amt - 1)
    } yield h +: t

  val TOTAL_WAVES = ENOUGH_AMT

  def accsAndGenesis(): Gen[(Seq[PrivateKeyAccount], PrivateKeyAccount, Block, Int)] = for {
    alice <- accountGen
    bob <- accountGen
    charlie <- accountGen
    dave <- accountGen
    miner <- accountGen
    ts <- positiveIntGen
    genesis1: GenesisTransaction = GenesisTransaction.create(alice, TOTAL_WAVES / 4, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(bob, TOTAL_WAVES / 4, ts + 1).explicitGet()
    genesis3: GenesisTransaction = GenesisTransaction.create(charlie, TOTAL_WAVES / 4, ts + 2).explicitGet()
    genesis4: GenesisTransaction = GenesisTransaction.create(dave, TOTAL_WAVES / 4, ts + 4).explicitGet()
  } yield (Seq(alice, bob, charlie, dave), miner, customBuildBlockOfTxs(randomSig, Seq(genesis1, genesis2, genesis3, genesis4), defaultSigner, 1, ts), ts)


  def g(totalTxs: Int, totalScenarios: Int): Gen[(Block, Seq[(BlockAndMicroblockSequence, Block)])] = for {
    aaa@(accs, miner, genesis, ts) <- accsAndGenesis()
    payments: Seq[TransferTransaction] <- randomPayments(accs, ts, totalTxs)
    intSeqs: Seq[BlockAndMicroblockSizes] <- randomSequences(totalTxs, totalScenarios)
  } yield {
    val version = 3: Byte
    val blocksAndMicros = intSeqs.map { intSeq =>
      val blockAndMicroblockSequence = r(payments, intSeq, genesis.uniqueId, miner, version, ts)
      val ref = bestRef(blockAndMicroblockSequence.last)
      val lastBlock = customBuildBlockOfTxs(ref, Seq.empty, miner, version, ts)
      (blockAndMicroblockSequence, lastBlock)
    }
    (genesis, blocksAndMicros)
  }
}

object BlockchainUpdaterBlockMicroblockSequencesSameTransactionsTest {

  def genSizes(total: Int): Gen[Seq[Int]] = for {
    h <- Gen.choose(1, total)
    t <- if (h < total) genSizes(total - h) else Gen.const(Seq.empty)
  } yield h +: t

  def genSplitSizes(total: Int): Gen[(Int, Seq[Int])] = genSizes(total).map { case (h :: tail) => (h, tail) }

  type BlockAndMicroblockSize = (Int, Seq[Int])
  type BlockAndMicroblockSizes = Seq[BlockAndMicroblockSize]
  type BlockAndMicroblocks = (Block, Seq[MicroBlock])
  type BlockAndMicroblockSequence = Seq[BlockAndMicroblocks]

  def randomSizeSequence(total: Int): Gen[BlockAndMicroblockSizes] = for {
    totalStep <- Gen.choose(1, Math.min(Math.min(total / 3 + 2, total), 250))
    h <- genSplitSizes(totalStep)
    t <- if (totalStep < total) randomSizeSequence(total - totalStep) else Gen.const(Seq.empty)
  } yield h +: t

  def randomSequences(total: Int, sequences: Int): Gen[Seq[BlockAndMicroblockSizes]] =
    if (sequences == 0)
      Gen.const(Seq.empty)
    else for {
      h <- randomSizeSequence(total)
      t <- randomSequences(total, sequences - 1)
    } yield h +: t

  def take(txs: Seq[Transaction], sizes: BlockAndMicroblockSize): ((Seq[Transaction], Seq[Seq[Transaction]]), Seq[Transaction]) = {
    val (blockAmt, microsAmts) = sizes
    val (blockTxs, rest) = txs.splitAt(blockAmt)
    val (reversedMicroblockTxs, res) = microsAmts.foldLeft((Seq.empty[Seq[Transaction]], rest)) { case ((acc, pool), amt) =>
      val (step, next) = pool.splitAt(amt)
      (step +: acc, next)
    }
    ((blockTxs, reversedMicroblockTxs.reverse), res)
  }

  def stepR(txs: Seq[Transaction], sizes: BlockAndMicroblockSize, prev: ByteStr, signer: PrivateKeyAccount, version: Byte, timestamp: Long): (BlockAndMicroblocks, Seq[Transaction]) = {
    val ((blockTxs, microblockTxs), rest) = take(txs, sizes)
    (chainBaseAndMicro(prev, blockTxs, microblockTxs, signer, version, timestamp), rest)
  }

  def bestRef(r: BlockAndMicroblocks): ByteStr = r._2.lastOption match {
    case Some(mb) => mb.totalResBlockSig
    case None => r._1.uniqueId
  }

  def r(txs: Seq[Transaction], sizes: BlockAndMicroblockSizes, initial: ByteStr, signer: PrivateKeyAccount, version: Byte, timestamp: Long): BlockAndMicroblockSequence = {
    sizes.foldLeft((Seq.empty[BlockAndMicroblocks], txs)) { case ((acc, rest), s) =>
      val prev = acc.headOption.map(bestRef).getOrElse(initial)
      val (step, next) = stepR(rest, s, prev, signer, version, timestamp)
      (step +: acc, next)
    }._1.reverse
  }
}
