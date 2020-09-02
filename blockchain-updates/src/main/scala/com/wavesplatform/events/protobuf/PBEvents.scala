package com.wavesplatform.events.protobuf

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append => PBAppend, Rollback => PBRollback}
import com.wavesplatform.events.protobuf.StateUpdate.{AssetStateUpdate, BalanceUpdate => PBBalanceUpdate, DataEntryUpdate => PBDataEntryUpdate, LeasingUpdate => PBLeasingUpdate}
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Transaction

object PBEvents {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def protobuf(event: events.BlockchainUpdated): BlockchainUpdated =
    event match {
      case events.BlockAppended(sig, height, block, updatedWavesAmount, blockStateUpdate, transactionStateUpdates) =>
        val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdate)
        val txsUpdates  = transactionStateUpdates.map(protobufStateUpdate)

        BlockchainUpdated(
          id = sig.toByteString,
          height = height,
          update = BlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = getIds(block.transactionData),
              stateUpdate = blockUpdate,
              transactionStateUpdates = txsUpdates,
              body = PBAppend.Body.Block(
                PBAppend.BlockAppend(
                  block = Some(PBBlocks.protobuf(block)),
                  updatedWavesAmount = updatedWavesAmount
                )
              )
            )
          )
        )
      case events.MicroBlockAppended(totalBlockId, height, microBlock, microBlockStateUpdate, transactionStateUpdates) =>
        val microBlockUpdate = Some(microBlockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdate)
        val txsUpdates       = transactionStateUpdates.map(protobufStateUpdate)

        BlockchainUpdated(
          id = totalBlockId.toByteString,
          height = height,
          update = BlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = getIds(microBlock.transactionData),
              stateUpdate = microBlockUpdate,
              transactionStateUpdates = txsUpdates,
              body = PBAppend.Body.MicroBlock(
                PBAppend.MicroBlockAppend(
                  microBlock = Some(PBMicroBlocks.protobuf(microBlock, totalBlockId))
                )
              )
            )
          )
        )
      case events.RollbackCompleted(to, height) =>
        BlockchainUpdated(
          id = to.toByteString,
          height = height,
          update = BlockchainUpdated.Update.Rollback(
            PBRollback(PBRollback.RollbackType.BLOCK)
          )
        )
      case events.MicroBlockRollbackCompleted(toSig, height) =>
        BlockchainUpdated(
          id = toSig.toByteString,
          height = height,
          update = BlockchainUpdated.Update.Rollback(
            PBRollback(PBRollback.RollbackType.MICROBLOCK)
          )
        )
    }

  private def toString(bytes: ByteStr): String = new String(bytes.arr, StandardCharsets.UTF_8)

  private def protobufAssetStateUpdate(a: events.AssetStateUpdate): AssetStateUpdate =
    AssetStateUpdate(
      assetId = a.asset.id.toByteString,
      decimals = a.decimals,
      name = toString(a.name),
      description = toString(a.description),
      reissuable = a.reissuable,
      volume = a.volume.longValue,
      script = PBTransactions.toPBScript(a.script.map(_.script)),
      sponsorship = a.sponsorship.getOrElse(0),
      nft = a.nft,
      assetExistedBefore = a.assetExistedBefore,
      safeVolume = ByteString.copyFrom(a.volume.toByteArray)
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

  private def getIds(txs: Seq[Transaction]): Seq[ByteString] = txs.map(t => ByteString.copyFrom(t.id().arr))
}
