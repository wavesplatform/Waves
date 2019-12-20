package com.wavesplatform.events.protobuf

import com.wavesplatform.events
import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.StateUpdate.AssetStateUpdate
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.events.protobuf.StateUpdate.{
  BalanceUpdate => PBBalanceUpdate,
  DataEntryUpdate => PBDataEntryUpdate,
  LeasingUpdate => PBLeasingUpdate
}
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append => PBAppend, Rollback => PBRollback}
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

  private def protobufAssetStateUpdate(a: events.AssetStateUpdate): AssetStateUpdate =
    AssetStateUpdate(
      a.asset.id,
      `type` = a match {
        case events.Issue(_, name, description, decimals, reissuable, volume, script, nft) =>
          AssetStateUpdate.Type.Issue(
            AssetStateUpdate.Issue(
              nameDescription = unsafeProtobufAssetNameDescription(name, description),
              decimals = decimals,
              reissuable = reissuable,
              volume = volume,
              script = script.map(PBTransactions.toPBScript),
              nft = nft
            )
          )
        case events.UpdateAssetVolume(_, volume) =>
          AssetStateUpdate.Type.UpdateAssetVolume(
            AssetStateUpdate.UpdateAssetVolume(
              newVolume = volume.longValue,
              safeNewVolume = ByteString.copyFrom(volume.toByteArray)
            )
          )
        case events.ForbidReissue(_) => AssetStateUpdate.Type.ForbidReissue(AssetStateUpdate.ForbidReissue())
        case events.SetSponsorship(_, sponsorship) =>
          AssetStateUpdate.Type.SetSponsorship(AssetStateUpdate.SetSponsorship(sponsorship = sponsorship))
        case events.SetAssetScript(_, script) =>
          AssetStateUpdate.Type.SetAssetScript(AssetStateUpdate.SetAssetScript(script = script.map(PBTransactions.toPBScript)))
        case events.UpdateAssetInfo(_, name, description) =>
          AssetStateUpdate.Type.UpdateAssetInfo(
            AssetStateUpdate.UpdateAssetInfo(newNameDescription = Some(AssetStateUpdate.NameDescriptionString(name, description)))
          )
      }
    )

  // @todo refactor after better name/description typing in node
  private def unsafeProtobufAssetNameDescription(
      name: Either[ByteStr, String],
      description: Either[ByteStr, String]
  ): AssetStateUpdate.Issue.NameDescription = {
    (name, description) match {
      case (Right(n), Right(d)) => AssetStateUpdate.Issue.NameDescription.String(AssetStateUpdate.NameDescriptionString(n, d))
      case (Left(n), Left(d))   => AssetStateUpdate.Issue.NameDescription.Bytes(AssetStateUpdate.NameDescriptionBytes(n, d))
      case _                    => throw new IllegalArgumentException("Asset name and description have different types")
    }
  }

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
