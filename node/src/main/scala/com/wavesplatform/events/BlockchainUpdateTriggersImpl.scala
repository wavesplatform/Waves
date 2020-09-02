package com.wavesplatform.events

import cats.syntax.monoid._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, Blockchain, Diff, DiffToStateApplier}
import com.wavesplatform.transaction.{Asset, GenesisTransaction, Transaction}
import monix.reactive.Observer

import scala.collection.mutable.ArrayBuffer

class BlockchainUpdateTriggersImpl(private val events: Observer[BlockchainUpdated]) extends BlockchainUpdateTriggers {

  override def onProcessBlock(block: Block, diff: DetailedDiff, minerReward: Option[Long], blockchainBefore: Blockchain): Unit = {
    val (blockStateUpdate, txsStateUpdates) = containerStateUpdate(blockchainBefore, diff, block.transactionData)

    // updatedWavesAmount can change as a result of either genesis transactions or miner rewards
    val updatedWavesAmount = blockchainBefore.height match {
      // genesis case
      case 0 => block.transactionData.collect { case GenesisTransaction(_, amount, _, _, _) => amount }.sum
      // miner reward case
      case _ => blockchainBefore.wavesAmount(blockchainBefore.height).toLong + minerReward.getOrElse(0L)
    }

    events.onNext(BlockAppended(block.signature, blockchainBefore.height + 1, block, updatedWavesAmount, blockStateUpdate, txsStateUpdates))
  }

  override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain, totalBlockId: ByteStr): Unit = {
    val (microBlockStateUpdate, txsStateUpdates) = containerStateUpdate(blockchainBefore, diff, microBlock.transactionData)
    events.onNext(MicroBlockAppended(totalBlockId, blockchainBefore.height, microBlock, microBlockStateUpdate, txsStateUpdates))
  }

  override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = events.onNext(RollbackCompleted(toBlockId, toHeight))

  override def onMicroBlockRollback(toBlockId: ByteStr, height: Int): Unit =
    events.onNext(MicroBlockRollbackCompleted(toBlockId, height))

  private def atomicStateUpdate(blockchainBefore: Blockchain, diff: Diff, byTransaction: Option[Transaction]): StateUpdate = {
    val blockchainAfter = CompositeBlockchain(blockchainBefore, Some(diff))

    val PortfolioUpdates(updatedBalances, updatedLeases) = DiffToStateApplier.portfolios(blockchainBefore, diff)

    val balances = ArrayBuffer.empty[(Address, Asset, Long)]
    for ((address, assetMap) <- updatedBalances; (asset, balance) <- assetMap) balances += ((address, asset, balance))

    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map { case (_, entry) => (address, entry) }
    }

    val assets: Seq[AssetStateUpdate] = for {
      a <- (diff.issuedAssets.keySet ++ diff.updatedAssets.keySet ++ diff.assetScripts.keySet ++ diff.sponsorship.keySet).toSeq
      AssetDescription(
        _,
        _,
        name,
        description,
        decimals,
        reissuable,
        totalVolume,
        _,
        script,
        sponsorship,
        nft
      ) <- blockchainAfter.assetDescription(a).toSeq
      existedBefore = !diff.issuedAssets.isDefinedAt(a)
    } yield AssetStateUpdate(
      a,
      decimals,
      ByteStr(name.toByteArray),
      ByteStr(description.toByteArray),
      reissuable,
      totalVolume,
      script,
      if (sponsorship == 0) None else Some(sponsorship),
      nft,
      existedBefore
    )

    StateUpdate(balances.toSeq, updatedLeases.toSeq, dataEntries, assets)
  }

  private def containerStateUpdate(
      blockchainBefore: Blockchain,
      diff: DetailedDiff,
      transactions: Seq[Transaction]
  ): (StateUpdate, Seq[StateUpdate]) = {
    val DetailedDiff(parentDiff, txsDiffs) = diff
    val parentStateUpdate                  = atomicStateUpdate(blockchainBefore, parentDiff, None)

    val (txsStateUpdates, _) = txsDiffs
      .zip(transactions)
      .foldLeft((ArrayBuffer.empty[StateUpdate], parentDiff)) {
        case ((updates, accDiff), (txDiff, tx)) =>
          (
            updates += atomicStateUpdate(CompositeBlockchain(blockchainBefore, Some(accDiff)), txDiff, Some(tx)),
            accDiff.combine(txDiff)
          )
      }

    (parentStateUpdate, txsStateUpdates.toSeq)
  }
}
