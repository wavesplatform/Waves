package com.wavesplatform

import org.scalacheck.Gen
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.transaction.SignedTransaction

trait BlockGen extends TransactionGen {

  def blockGen(txs: Seq[SignedTransaction]): Gen[Block] = for {
    reference <- byteArrayGen(Block.BlockIdLength)
    baseTarget <- Gen.posNum[Long]
    generationSignature <- byteArrayGen(WavesConsensusModule.GeneratorSignatureLength)
    signer <- accountGen
  } yield Block.buildAndSign(1, txs.map(_.timestamp).max, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), txs, signer)


  val blockGen: Gen[Block] = for {
    txCount <- Gen.choose(10, 50)
    txs <- Gen.listOfN(txCount, randomTransactionGen)
    blk <- blockGen(txs)
  } yield blk

}
