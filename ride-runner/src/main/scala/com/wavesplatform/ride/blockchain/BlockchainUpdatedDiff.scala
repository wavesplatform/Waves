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
    assetDetails: Map[ByteString, WithHeight[StateUpdate.AssetStateUpdate]] = Map.empty,
    balances: Map[ByteString, WithHeight[StateUpdate.BalanceUpdate]] = Map.empty,
    leasingForAddress: Map[ByteString, WithHeight[StateUpdate.LeasingUpdate]] = Map.empty,
    dataEntries: Map[(ByteString, String), WithHeight[StateUpdate.DataEntryUpdate]] = Map.empty,
    updatedAccountScriptsByPk: Map[ByteString, WithHeight[ByteString]] = Map.empty,
    newTransactionIds: Set[WithHeight[ByteString]] = Set.empty,
    removedTransactionIds: Set[WithHeight[ByteString]] = Set.empty
)

object BlockchainUpdatedDiff {
  val emptyByteStrSet = Set.empty[WithHeight[ByteString]]

  def append(orig: BlockchainUpdatedDiff, event: BlockchainUpdated): BlockchainUpdatedDiff = {
    def withHeight[T](x: T): WithHeight[T] = WithHeight(event.height, x)

    val (stateUpdates, updatedAccountScripts, removedTxIds, newTxIds) = event.update match {
      case Update.Empty => (Nil.view, Map.empty[ByteString, WithHeight[ByteString]], emptyByteStrSet, emptyByteStrSet)

      case Update.Rollback(rollback) =>
        (
          Seq(rollback.getRollbackStateUpdate).view,
          Map.empty[ByteString, WithHeight[ByteString]],
          rollback.removedTransactionIds.map(withHeight).toSet,
          emptyByteStrSet
        )

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
                case Data.SetScript(txData) => (tx.senderPublicKey, withHeight(txData.script)).some
                case _                      => none
              }
            case _ => none
          }
          .toMap

        (
          Seq(append.getStateUpdate).view ++ append.transactionStateUpdates,
          updatedAccountScripts,
          emptyByteStrSet,
          append.transactionIds.map(withHeight).toSet
        )
    }

    orig.copy(
      newHeight = event.height,
      assetDetails =
        orig.assetDetails.combineByKeys(stateUpdates.flatMap(_.assets).map(x => x.after.get.assetId -> withHeight(x))) { (orig, update) =>
          update.copy(value = update.value.copy(before = orig.value.before))
        },
      balances = orig.balances.combineByKeys(stateUpdates.flatMap(_.balances).map(x => x.address -> withHeight(x))) { (orig, update) =>
        update.copy(value = update.value.copy(amountBefore = orig.value.amountBefore))
      },
      leasingForAddress = orig.leasingForAddress.combineByKeys(stateUpdates.flatMap(_.leasingForAddress).map(x => x.address -> withHeight(x))) {
        (orig, update) => update.copy(value = update.value.copy(inBefore = orig.value.inBefore, outBefore = orig.value.outBefore))
      },
      dataEntries = orig.dataEntries.combineByKeys(stateUpdates.flatMap(_.dataEntries).map(x => (x.address, getDataEntryKey(x)) -> withHeight(x))) {
        (orig, update) => update.copy(value = update.value.copy(dataEntryBefore = orig.value.dataEntryBefore))
      },
      updatedAccountScriptsByPk = orig.updatedAccountScriptsByPk.combineByKeys(updatedAccountScripts) { (_, update) => update },
      newTransactionIds = orig.newTransactionIds -- removedTxIds ++ newTxIds,
      removedTransactionIds = orig.removedTransactionIds -- newTxIds ++ removedTxIds
    )

  }

  def getDataEntryKey(x: StateUpdate.DataEntryUpdate): String = x.dataEntryBefore.orElse(x.dataEntry).get.key
}
