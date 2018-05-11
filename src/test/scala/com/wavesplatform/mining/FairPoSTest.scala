package com.wavesplatform.mining

import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.state.{ByteStr, _}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.joda.time.DateTime
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.lagonaki.mocks.TestBlock

import scala.util.Random

class FairPoSTest extends PropSpec with Matchers with PropertyChecks with TransactionGen with NoShrink {
  val pos = FairPoSCalculator

  val balance1 = 1000l
  val balance2 = 15000l
  val balance3 = 20000l

  val balances = 1000 to 20000 by 100 toList
  val btargets = 10 to 50 toList

  def randomSig: ByteStr = TestBlock.randomOfLength(Block.BlockIdLength)

  def generationSignature: ByteStr = {
    val arr = Array.fill(Block.GeneratorSignatureLength)(0: Byte)
    Random.nextBytes(arr)
    ByteStr(arr)
  }

  property("pam pam") {
    val result = for {
      target <- btargets
      balanceToHitAndTime = balances.map(b => {
        val currentTime = System.currentTimeMillis()
        val (hit, ts) = nextBlockGeneratingTime(target, b).explicitGet()
        b -> (hit, ts - currentTime)
      })
      miner = balanceToHitAndTime.minBy(_._2._2)
    } yield target -> miner

    println(result mkString "\n")

    true shouldBe true
  }

  def createAccount(): PrivateKeyAccount = {
    val seed = Array.fill(32)(0: Byte)
    Random.nextBytes(seed)
    PrivateKeyAccount(seed)
  }

  def nextBlockGeneratingTime(btarget: Long, balance: Long): Either[String, (BigInt, Long)] = {

    val acc = createAccount()

    val block = Block
      .buildAndSign(
        version = 3,
        timestamp = System.currentTimeMillis(),
        reference = randomSig,
        consensusData = NxtLikeConsensusBlockData(baseTarget = btarget, generationSignature = generationSignature),
        transactionData = Seq.empty,
        signer = acc,
        Set.empty
      )
      .explicitGet()

    val cData        = block.consensusData
    val s            = pos.generatorSignature(cData.generationSignature.arr, acc.publicKey)
    val h            = pos.hit(s)
    val t            = cData.baseTarget
    val calculatedTs = pos.time(h, t, balance) + block.timestamp
    if (0 < calculatedTs && calculatedTs < Long.MaxValue) {
      Right((h, calculatedTs))
    } else
      Left(s"Invalid next block generation time: $calculatedTs")
  }
}
