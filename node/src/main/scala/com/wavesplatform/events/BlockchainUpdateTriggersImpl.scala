package com.wavesplatform.events

import cats.syntax.monoid._
import com.wavesplatform.account.Address
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.DiffToStateApplier.PortfolioUpdates
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{AccountDataInfo, Blockchain, Diff, DiffToStateApplier}
import com.wavesplatform.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.reactive.Observer

import scala.collection.mutable.ArrayBuffer

class BlockchainUpdateTriggersImpl(private val events: Observer[BlockchainUpdated]) extends BlockchainUpdateTriggers {

  override def onProcessBlock(block: Block, diff: DetailedDiff, blockchainBefore: Blockchain): Unit = {
    val (blockStateUpdate, txsStateUpdates) = containerStateUpdate(blockchainBefore, diff, block.transactionData)
    events.onNext(BlockAppended(block, blockchainBefore.height + 1, blockStateUpdate, txsStateUpdates))
  }

  override def onProcessMicroBlock(microBlock: MicroBlock, diff: DetailedDiff, blockchainBefore: Blockchain): Unit = {
    val (microBlockStateUpdate, txsStateUpdates) = containerStateUpdate(blockchainBefore, diff, microBlock.transactionData)
    events.onNext(MicroBlockAppended(microBlock, blockchainBefore.height, microBlockStateUpdate, txsStateUpdates))
  }

  override def onRollback(toBlockId: ByteStr, toHeight: Int): Unit = events.onNext(RollbackCompleted(toBlockId, toHeight))

  override def onMicroBlockRollback(toTotalResBlockSig: ByteStr): Unit = events.onNext(MicroBlockRollbackCompleted(toTotalResBlockSig))

  private def getIssuedAssets(diff: Diff): Seq[Issue] =
    for {
      (issuedAsset, (staticInfo, info, volumeInfo)) <- diff.issuedAssets.toSeq
      script = diff.assetScripts.get(issuedAsset).flatten.map(_._2)
    } yield Issue(
      issuedAsset,
      info.name,
      info.description,
      staticInfo.decimals,
      volumeInfo.isReissuable,
      volumeInfo.volume.longValue,
      script,
      staticInfo.nft
    )

  private def getAssetVolumeUpdates(diff: Diff): Seq[UpdateAssetVolume] =
    for {
      ua <- diff.updatedAssets.toSeq
      v  <- ua._2.right.toSeq
    } yield UpdateAssetVolume(
      ua._1,
      v.volume
    )

  private def atomicStateUpdate(blockchainBefore: Blockchain, diff: Diff, byTransaction: Option[Transaction]): StateUpdate = {
    val PortfolioUpdates(updatedBalances, updatedLeases) = DiffToStateApplier.portfolios(blockchainBefore, diff)

    val balances = ArrayBuffer.empty[(Address, Asset, Long)]
    for ((address, assetMap) <- updatedBalances; (asset, balance) <- assetMap) balances += ((address, asset, balance))

    val dataEntries = diff.accountData.toSeq.flatMap {
      case (address, AccountDataInfo(data)) =>
        data.toSeq.map { case (_, entry) => (address, entry) }
    }

    val assets: Seq[AssetStateUpdate] = byTransaction match {
      case Some(tx) =>
        tx match {
          case _: IssueTransaction => getIssuedAssets(diff)

          case _: ReissueTransaction =>
            val volumeUpdates = getAssetVolumeUpdates(diff)
            // reissuable: false in ReissueTransaction means that reissue has been forbidden
            val reissueForbidden = for {
              (a, info) <- diff.updatedAssets.headOption.toSeq
              v         <- info.right.toSeq if (!v.isReissuable)
            } yield ForbidReissue(a)
            volumeUpdates ++ reissueForbidden

          case _: BurnTransaction => getAssetVolumeUpdates(diff)

          case _: InvokeScriptTransaction =>
            // inferring what happened in the invoke
            val issued        = getIssuedAssets(diff)
            val volumeUpdates = getAssetVolumeUpdates(diff)
            val reissueForbidden = for {
              (a, info) <- diff.updatedAssets.toSeq
              v         <- info.right.toSeq
              // is asset became non-reissuable after the invoke, but was reissuable before
              if (!v.isReissuable &&
                blockchainBefore.assetDescription(a).forall(_.reissuable))
            } yield ForbidReissue(a)
            issued ++ volumeUpdates ++ reissueForbidden
        }
      case None => Seq.empty
    }

    StateUpdate(balances, updatedLeases.toSeq, dataEntries, assets)
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

    (parentStateUpdate, txsStateUpdates)
  }
}
