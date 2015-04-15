package scorex.consensus.nxt

import ntp.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.consensus.BlockGenerationFunctions
import scorex.crypto.{Base58, Crypto}

import scala.util.Random

object NxtBlockGenerationFunctions extends BlockGenerationFunctions {
  private val AvgFrequency = 60 //the algo's goal is 1 block per minute in average

  override protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub] = ???

  private def generatorSignature(lastBlockGeneratorSignature:Array[Byte], generator: PrivateKeyAccount) =
    Crypto.sha256(lastBlockGeneratorSignature ++ generator.publicKey)

  private def hit(lastBlockData: NxtBlockGenerationData, generator: PrivateKeyAccount): BigInt =
    BigInt(1, generatorSignature(lastBlockData.generatorSignature, generator).take(8))

  private def baseTarget(lastBlockData: NxtBlockGenerationData, lastBlockTimestamp: Long): Long = {
    val eta = (NTP.getTime - lastBlockTimestamp) / 1000 //in seconds
    val prevBt = BigInt(lastBlockData.baseTarget)
    val t = bound(prevBt * eta / AvgFrequency, prevBt / 2, prevBt * 2)
    bound(t, 1, Long.MaxValue).toLong
  }

  private def target(lastBlockData: NxtBlockGenerationData, lastBlockTimestamp: Long, generator: PrivateKeyAccount): BigInt = {
    val eta = (NTP.getTime - lastBlockTimestamp) / 1000 //in seconds
    val effBalance:BigDecimal = 10000000// generator.generatingBalance
    (lastBlockData.baseTarget * eta * effBalance).toBigInt()
  }

  private def bound(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value

  /*
  todo: remove testing code
  def main(args:Array[String]): Unit ={
    val seed = Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz")
    val acc = new PrivateKeyAccount(seed)
    val ts = System.currentTimeMillis() - 10000

    (1 to 1000).flatMap{_ =>
        val data = new NxtBlockGenerationData(307445734, Crypto.sha256(Random.nextString(5).getBytes))
        val h = hit(data, acc)
        val t = target(data, ts, acc)
        val bt = baseTarget(data, ts)
        if(h<t) Some((h,t,bt)) else None
    }.foreach{case (h,t,bt) => println(s"hit: $h target: $t, next base: $bt generation: ${h<t}`")}
  } */
}