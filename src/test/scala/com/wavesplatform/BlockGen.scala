package com.wavesplatform

import org.scalacheck.Gen
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.{SignedTransaction, SimpleTransactionModule, Transaction}

trait BlockGen extends TransactionGen {

  import BlockGen._

  val blockParamGen: Gen[(Seq[SignedTransaction], PrivateKeyAccount)] = for {
    count <- Gen.choose(minTransactionsInBlockCount, maxTransactionsInBlockCount)
    transactions <- randomTransactionsGen(count)
    signer <- accountGen
  } yield (transactions, signer)

  def blockGen(txs: Seq[Transaction], signer: PrivateKeyAccount): Gen[Block] = for {
    reference <- byteArrayGen(Block.BlockIdLength)
    baseTarget <- Gen.posNum[Long]
    generationSignature <- byteArrayGen(Block.GeneratorSignatureLength)
  } yield Block.buildAndSign(1, txs.map(_.timestamp).max, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), txs, signer)

  val randomSignerBlockGen: Gen[Block] = for {
    (transactions, signer) <- blockParamGen
    block <- blockGen(transactions, signer)
  } yield block

  val predefinedSignerBlockGen: Gen[Block] = for {
    (transactions, _) <- blockParamGen
    signer <- Gen.const(predefinedSignerPrivateKey)
    block <- blockGen(transactions, signer)
  } yield block

  val mixedBlockGen: Gen[Block] = for {
    block <- Gen.oneOf(randomSignerBlockGen, predefinedSignerBlockGen)
  } yield block

  val randomBlocksSeqGen: Gen[(Int, Int, Seq[Block])] = for {
    start <- Gen.posNum[Int].label("from")
    end <- Gen.chooseNum(start, start + 20).label("to")
    blockCount <- Gen.choose(0, end - start + 1).label("actualBlockCount")
    blocks <- Gen.listOfN(blockCount, randomSignerBlockGen).label("blocks")
  } yield (start, end, blocks)

  val mixedBlocksSeqGen: Gen[(Int, Int, Seq[Block])] = for {
    start <- Gen.posNum[Int].label("from")
    end <- Gen.chooseNum(start, start + 20).label("to")
    blockCount <- Gen.choose(0, end - start + 1).label("actualBlockCount")
    blocks <- Gen.listOfN(blockCount, mixedBlockGen).label("blocks")
  } yield (start, end, blocks)

}

object BlockGen {
  val minTransactionsInBlockCount = 1
  val maxTransactionsInBlockCount = 100
  val predefinedSignerPrivateKey: PrivateKeyAccount = PrivateKeyAccount((1 to 10).map(_.toByte).toArray)
}
