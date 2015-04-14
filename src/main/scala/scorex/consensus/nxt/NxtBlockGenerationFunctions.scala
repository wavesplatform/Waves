package scorex.consensus.nxt

import ntp.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{Block, BlockStub}
import scorex.consensus.BlockGenerationFunctions
import scorex.crypto.Crypto

object NxtBlockGenerationFunctions extends BlockGenerationFunctions {
  private val AvgFrequency = 60 //the algo's goal is 1 block per minute in average

  override protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub] = ???

  private def hit(lastBlockData: NxtBlockGenerationData, generator: PrivateKeyAccount): Long = {
    val hash = Crypto.sha256(lastBlockData.generatorSignature ++ generator.publicKey)
    BigInt(hash.take(8)).toLong
  }

  private def baseTarget(lastBlockData: NxtBlockGenerationData, lastBlockTimestamp: Long): Long = {
    val eta = (NTP.getTime - lastBlockTimestamp) / 1000 //in seconds
    val prevBt = BigInt(lastBlockData.baseTarget)
    val t = bound(prevBt * eta / AvgFrequency, prevBt / 2, prevBt * 2)
    bound(t, 1, Long.MaxValue).toLong
  }

  private def target(lastBlockData: NxtBlockGenerationData, lastBlockTimestamp: Long, generator: PrivateKeyAccount): Long = {
    val eta = (NTP.getTime - lastBlockTimestamp) / 1000 //in seconds
    bound((lastBlockData.baseTarget * eta * generator.generatingBalance).toBigInt(), 1, Long.MaxValue).toLong
  }

  private def bound(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value
}