package scorex.consensus.nxt

import scorex.utils.{ScorexLogging, NTP}
import scorex.account.{PublicKeyAccount, PrivateKeyAccount}
import scorex.block.{Block, BlockStub}
import scorex.consensus.BlockGenerationFunctions
import scorex.crypto.Crypto
import scorex.settings.Constants

object NxtBlockGenerationFunctions extends BlockGenerationFunctions with ScorexLogging {
  val AvgFrequency = 2 //60 - the algo's goal is 1 block per minute in average

  override protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub] = {
    val lastBlockKernelData = lastBlock.generationData.asInstanceOf[NxtBlockGenerationData]
    val lastBlockTime = lastBlock.timestamp

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account)

    val eta = (NTP.correctedTime() - lastBlock.timestamp) / 1000
    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, account balance: ${account.generatingBalance}")
    if (h < t) {
      val ts = NTP.correctedTime()
      val btg = calcBaseTarget(lastBlockKernelData, lastBlockTime, ts)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      Some(BlockStub(Block.Version, lastBlock.signature, ts, account,
        new NxtBlockGenerationData(btg, gs).asInstanceOf[Constants.ConsensusAlgo.kernelData]))
    } else None
  }

  private[nxt] def calcGeneratorSignature(lastBlockData: NxtBlockGenerationData, generator: PublicKeyAccount) =
    Crypto.sha256(lastBlockData.generatorSignature ++ generator.publicKey)

  private[nxt] def calcHit(lastBlockData: NxtBlockGenerationData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8))

  private[nxt] def calcBaseTarget(lastBlockData: NxtBlockGenerationData,
                         lastBlockTimestamp: Long,
                         currentTime: Long): Long = {
    val eta = (currentTime - lastBlockTimestamp) / 1000 //in seconds
    val prevBt = BigInt(lastBlockData.baseTarget)
    val t = bound(prevBt * eta / AvgFrequency, prevBt / 2, prevBt * 2)
    bound(t, 1, Long.MaxValue).toLong
  }

  private[nxt] def calcTarget(lastBlockData: NxtBlockGenerationData,
                              lastBlockTimestamp: Long,
                              generator:PublicKeyAccount): BigInt = {
    val eta = (NTP.correctedTime() - lastBlockTimestamp) / 1000 //in seconds
    val effBalance: BigDecimal = generator.generatingBalance
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