package com.wavesplatform.ride.blockchain

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.events.protobuf.{BlockchainUpdated, StateUpdate}
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data

case class BlockchainUpdatedDiff(
    newHeight: Int = 0,
    assets: Map[ByteString, StateUpdate.AssetStateUpdate] = Map.empty,
    balances: Map[ByteString, StateUpdate.BalanceUpdate] = Map.empty,
    leasingForAddress: Map[ByteString, StateUpdate.LeasingUpdate] = Map.empty,
    dataEntries: Map[(ByteString, String), StateUpdate.DataEntryUpdate] = Map.empty,
    updatedAccountScriptsByPk: Map[ByteString, ByteString] = Map.empty, // PK -> script, TODO: txId?
    newTransactionIds: Seq[ByteString] = Nil,
    removedTransactionIds: Seq[ByteString] = Nil
)

object BlockchainUpdatedDiff {
  def append(orig: BlockchainUpdatedDiff, event: BlockchainUpdated): BlockchainUpdatedDiff = {
    val (stateUpdates, updatedAccountScripts, removedTxIds, newTxIds) = event.update match {
      case Update.Empty => (Nil.view, Map.empty[ByteString, ByteString], Nil, Nil)

      case Update.Rollback(rollback) =>
        (Seq(rollback.getRollbackStateUpdate).view, Map.empty[ByteString, ByteString], rollback.removedTransactionIds, Nil)

      case Update.Append(append) =>
        val txs = append.body match {
          case Body.Block(block)           => block.getBlock.transactions
          case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
          case Body.Empty                  => Nil
        }

        val updatedAccountScripts = txs
          .map(_.transaction)
          .flatMap {
            case Transaction.WavesTransaction(tx) =>
              tx.data match {
                case Data.SetScript(txData) => (tx.senderPublicKey, txData.script).some
                case _                      => none
              }
            case _ => none
          }
          .toMap

        (Seq(append.getStateUpdate).view ++ append.transactionStateUpdates, updatedAccountScripts, Nil, append.transactionIds)
    }

    orig.copy(
      newHeight = event.height,
      assets = orig.assets.combineByKeys(stateUpdates.flatMap(_.assets).map(x => x.after.get.assetId -> x)) { (orig, update) =>
        orig.copy(after = update.after)
      },
      balances = orig.balances.combineByKeys(stateUpdates.flatMap(_.balances).map(x => x.address -> x)) { (orig, update) =>
        orig.copy(amountAfter = update.amountAfter)
      },
      leasingForAddress = orig.leasingForAddress.combineByKeys(stateUpdates.flatMap(_.leasingForAddress).map(x => x.address -> x)) { (orig, update) =>
        orig.copy(inAfter = update.inAfter, outAfter = update.outAfter)
      },
      dataEntries =
        orig.dataEntries.combineByKeys(stateUpdates.flatMap(_.dataEntries).map(x => (x.address, getDataEntryKey(x)) -> x)) { (orig, update) =>
          orig.copy(dataEntry = update.dataEntry)
        },
      updatedAccountScriptsByPk = orig.updatedAccountScriptsByPk.combineByKeys(updatedAccountScripts) { (_, update) => update },
      newTransactionIds = newTxIds,
      removedTransactionIds = removedTxIds
    )

  }

  def getDataEntryKey(x: StateUpdate.DataEntryUpdate): String = x.dataEntryBefore.orElse(x.dataEntry).get.key
}
