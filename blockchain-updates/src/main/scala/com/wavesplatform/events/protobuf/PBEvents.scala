package com.wavesplatform.events.protobuf

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append => PBAppend, Rollback => PBRollback}
import com.wavesplatform.events.protobuf.StateUpdate.{
  AssetStateUpdate,
  BalanceUpdate => PBBalanceUpdate,
  DataEntryUpdate => PBDataEntryUpdate,
  LeasingUpdate => PBLeasingUpdate
}
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Transaction

object PBEvents {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def protobuf(event: events.BlockchainUpdated): BlockchainUpdated =
    event match {
      case events.BlockAppended(sig, height, block, blockStateUpdate, transactionStateUpdates) =>
        val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdate)
        val txsUpdates  = transactionStateUpdates.map(protobufStateUpdate)

        BlockchainUpdated(
          id = sig,
          height = height,
          update = BlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = getIds(block.transactionData),
              stateUpdate = blockUpdate,
              transactionStateUpdates = txsUpdates,
              body = PBAppend.Body.Block(PBBlocks.protobuf(block))
            )
          )
        )
      case events.MicroBlockAppended(sig, height, microBlock, microBlockStateUpdate, transactionStateUpdates) =>
        val microBlockUpdate = Some(microBlockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdate)
        val txsUpdates       = transactionStateUpdates.map(protobufStateUpdate)

        BlockchainUpdated(
          id = sig,
          height = height,
          update = BlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = getIds(microBlock.transactionData),
              stateUpdate = microBlockUpdate,
              transactionStateUpdates = txsUpdates,
              body = PBAppend.Body.MicroBlock(PBMicroBlocks.protobuf(microBlock))
            )
          )
        )
      case events.RollbackCompleted(to, height) =>
        BlockchainUpdated(
          id = to,
          height = height,
          update = BlockchainUpdated.Update.Rollback(PBRollback.BLOCK)
        )
      case events.MicroBlockRollbackCompleted(toSig, height) =>
        BlockchainUpdated(
          id = toSig,
          height = height,
          update = BlockchainUpdated.Update.Rollback(PBRollback.MICROBLOCK)
        )
    }

  private def toString(bytesOrString: Either[ByteStr, String]): String =
    bytesOrString match {
      case Left(bytes) => new String(bytes, StandardCharsets.UTF_8)
      case Right(s)    => s
    }

  private def protobufAssetStateUpdate(a: events.AssetStateUpdate): AssetStateUpdate =
    AssetStateUpdate(
      assetId = a.asset.id,
      decimals = a.decimals,
      name = toString(a.name),
      description = toString(a.description),
      reissuable = a.reissuable,
      volume = a.volume,
      script = a.script.map(PBTransactions.toPBScript),
      sponsorship = a.sponsorship.getOrElse(0),
      nft = a.nft,
      assetExistedBefore = a.assetExistedBefore
    )

  private def protobufStateUpdate(su: events.StateUpdate): StateUpdate = {
    StateUpdate(
      balances = su.balances.map {
        case (addr, assetId, amt) =>
          PBBalanceUpdate(address = addr, amount = Some((assetId, amt)))
      },
      leases = su.leases.map {
        case (addr, leaseBalance) =>
          PBLeasingUpdate(address = addr, in = leaseBalance.in, out = leaseBalance.out)
      },
      dataEntries = su.dataEntries.map {
        case (addr, entry) => PBDataEntryUpdate(address = addr, dataEntry = Some(PBTransactions.toPBDataEntry(entry)))
      },
      assets = su.assets.map(protobufAssetStateUpdate)
    )
  }

  private def getIds(txs: Seq[Transaction]): Seq[ByteString] = txs.map(t => ByteString.copyFrom(t.id().arr)),
}
