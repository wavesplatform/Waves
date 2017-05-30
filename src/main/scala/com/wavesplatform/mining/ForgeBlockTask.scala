package com.wavesplatform.mining

import java.util.concurrent.Callable

import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.transaction.UnconfirmedTransactionsStorage
import scorex.transaction.UnconfirmedTransactionsStorage.packUnconfirmed
import scorex.utils.{ScorexLogging, Time}

class ForgeBlockTask(
    parentHeight: Int,
    parent: Block,
    greatGrandParent: Option[Block],
    state: StateReader,
    utx: UnconfirmedTransactionsStorage,
    time: Time,
    bcs: BlockchainSettings,
    account: PrivateKeyAccount) extends Callable[Option[Block]] with ScorexLogging {

  import ForgeBlockTask._
  import scorex.transaction.PoSCalc._

  override def call(): Option[Block] = try {
    val balance = generatingBalance(state, bcs.functionalitySettings)(account, parentHeight)

    if (balance < MinimalEffectiveBalanceForGenerator) {
      throw new IllegalStateException(s"Effective balance $balance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    val lastBlockKernelData = parent.consensusData
    val currentTime = time.correctedTime()

    val h = calcHit(lastBlockKernelData, account)
    val t = calcTarget(parent, currentTime, balance)

    val eta = (currentTime - parent.timestamp) / 1000

    log.debug(s"hit=$h, target=$t, ${if (h < t) "" else "NOT"} generating, eta=$eta, account=$account, " +
      s"balance=$balance, lastBlockId=${parent.encodedId}, height=$parentHeight, lastTarget=${lastBlockKernelData.baseTarget}")

    if (h < t) {
      val avgBlockDelay = bcs.genesisSettings.averageBlockDelay
      val btg = calcBaseTarget(avgBlockDelay, parentHeight, parent, greatGrandParent, currentTime)
      val gs = calcGeneratorSignature(lastBlockKernelData, account)
      val consensusData = NxtLikeConsensusBlockData(btg, gs)

      val unconfirmed = packUnconfirmed(state, bcs.functionalitySettings, utx, time, parentHeight)
      log.debug(s"Building block with ${unconfirmed.size} transactions approx. $eta seconds after previous block")

      Some(Block.buildAndSign(Version,
        currentTime,
        parent.uniqueId,
        consensusData,
        unconfirmed,
        account))
    } else None
  } catch {
    case e: UnsupportedOperationException =>
      log.debug(s"DB can't find last block because of unexpected modification")
      None
    case e: IllegalStateException =>
      log.debug(s"Failed to generate new block: ${e.getMessage}")
      None
  }
}

object ForgeBlockTask {
  val Version: Byte = 2
}
