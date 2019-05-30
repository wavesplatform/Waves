package com.wavesplatform.events.protobuf

import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.events.protobuf.StateUpdate.{
  BalanceUpdate => PBBalanceUpdate,
  DataEntryUpdate => PBDataEntryUpdate,
  LeasingUpdate => PBLeasingUpdate
}

import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append => PBAppend, Rollback => PBRollback}

import com.wavesplatform.state.{BlockAppended, MicroBlockAppended, MicroBlockRollbackCompleted, RollbackCompleted}

object PBEvents {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  private def protobufStateUpdated(su: VanillaStateUpdate): PBStateUpdate = {
    PBStateUpdate(
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
      }
    )
  }

  def protobuf(event: VanillaBlockchainUpdated): PBBlockchainUpdated =
    event match {
      case BlockAppended(block, height, blockStateUpdate, transactionsStateUpdates) =>
        val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdated)
        val txsUpdates  = transactionsStateUpdates.map(protobufStateUpdated)

        PBBlockchainUpdated(
          id = block.uniqueId,
          height = height,
          update = PBBlockchainUpdated.Update.Append(
            PBAppend(
              stateUpdate = blockUpdate,
              transactionsStateUpdates = txsUpdates,
              body = PBAppend.Body.Block(PBBlocks.protobuf(block))
            )
          )
        )
      case MicroBlockAppended(microBlock, height, microBlockStateUpdate, transactionsStateUpdates) =>
        val microBlockUpdate = Some(microBlockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdated)
        val txsUpdates       = transactionsStateUpdates.map(protobufStateUpdated)

        PBBlockchainUpdated(
          id = microBlock.totalResBlockSig,
          height = height,
          update = PBBlockchainUpdated.Update.Append(
            PBAppend(
              stateUpdate = microBlockUpdate,
              transactionsStateUpdates = txsUpdates,
              body = PBAppend.Body.MicroBlock(PBMicroBlocks.protobuf(microBlock))
            )
          )
        )
      case RollbackCompleted(to, height) =>
        PBBlockchainUpdated(
          id = to,
          height = height,
          update = PBBlockchainUpdated.Update.Rollback(PBRollback.BLOCK)
        )
      case MicroBlockRollbackCompleted(to, height) =>
        PBBlockchainUpdated(
          id = to,
          height = height,
          update = PBBlockchainUpdated.Update.Rollback(PBRollback.MICROBLOCK)
        )
    }
}
