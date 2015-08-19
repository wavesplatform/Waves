package scorex.consensus.nxt

import scorex.Controller
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.block.{Block, BlockStub}
import scorex.consensus.BlockGenerationFunctions
import scorex.crypto.Crypto
import scorex.settings.Constants
import scorex.utils.{NTP, ScorexLogging}

object NxtBlockGenerationFunctions extends BlockGenerationFunctions with ScorexLogging {
  val AvgFrequency = 2 //60 - the algo's goal is 1 block per minute in average

  override protected def generateNextBlock(account: PrivateKeyAccount, lastBlock: Block): Option[BlockStub] = {
    val lastBlockKernelData = lastBlock.generationData.asInstanceOf[NxtBlockGenerationData]
    val lastBlockTime = lastBlock.timestamp

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(lastBlockKernelData, lastBlockTime, account)

    val eta = (NTP.correctedTime() - lastBlock.timestamp) / 1000
    log.debug(s"hit: $h, target: $t, generating ${h < t}, eta $eta, account balance: ${Controller.blockchainStorage.generationBalance(account)}")
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
    val t = bounded(prevBt * eta / AvgFrequency, prevBt / 2, prevBt * 2)
    bounded(t, 1, Long.MaxValue).toLong
  }

  private[nxt] def calcTarget(lastBlockData: NxtBlockGenerationData,
                              lastBlockTimestamp: Long,
                              generator: PublicKeyAccount): BigInt = {
    val eta = (NTP.correctedTime() - lastBlockTimestamp) / 1000 //in seconds
    val effBalance: BigDecimal = Controller.blockchainStorage.generationBalance(generator)
    (lastBlockData.baseTarget * eta * effBalance).toBigInt()
  }

  private def bounded(value: BigInt, min: BigInt, max: BigInt): BigInt =
    if (value < min) min else if (value > max) max else value
}