package com.wavesplatform.events.protobuf

import com.wavesplatform.common.state.ByteStr

import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions

import com.wavesplatform.events.protobuf.StateUpdated.ReasonType.{BLOCK, MICROBLOCK, TRANSACTION}
import com.wavesplatform.events.protobuf.StateUpdated.{
  BalanceUpdated => PBBalanceUpdated,
  LeasingUpdated => PBLeasingUpdated,
  DataEntryUpdated => PBDataEntryUpdated
}
import com.wavesplatform.state.{BlockAdded, MicroBlockAdded, MicroBlockRollbackCompleted, RollbackCompleted}

object PBEvents {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  private def protobufStateUpdated(reason: (Short, ByteStr), su: VanillaStateUpdated): PBStateUpdated = {
    val reasonType = reason._1 match {
      case 0 => BLOCK
      case 1 => TRANSACTION
      case 2 => MICROBLOCK
    }
    PBStateUpdated(
      reasonType = reasonType,
      reasonId = reason._2,
      balances = su.balances.map {
        case (addr, assetId, amt) =>
          PBBalanceUpdated(address = addr, amount = Some((assetId, amt)))
      },
      leases = su.leases.map {
        case (addr, leaseBalance) =>
          PBLeasingUpdated(address = addr, in = leaseBalance.in, out = leaseBalance.out)
      },
      dataEntries = su.dataEntries.map {
        case (addr, entry) => PBDataEntryUpdated(address = addr, dataEntry = Some(PBTransactions.toPBDataEntry(entry)))
      }
    )
  }

  def protobuf(event: VanillaBlockchainUpdated): PBBlockchainUpdated =
    event match {
      case BlockAdded(block, height, blockStateUpdate, transactionsStateUpdates) =>
        val txsUpdates = block.transactionData
          .map(_.id())
          .zip(transactionsStateUpdates)
          .map { case (id, su) => protobufStateUpdated((1, id), su) }

        val blockUpdate = protobufStateUpdated((0, block.uniqueId), blockStateUpdate)

        PBBlockchainUpdated(
          id = block.uniqueId,
          height = height,
          stateUpdates = Seq(blockUpdate) ++ txsUpdates,
          reason = PBBlockchainUpdated.Reason.Block(PBBlocks.protobuf(block)),
          `type` = PBBlockchainUpdated.UpdateType.BLOCK
        )
      case MicroBlockAdded(microBlock, height, microBlockStateUpdate, transactionsStateUpdates) =>
        val txsUpdates = microBlock.transactionData
          .map(_.id())
          .zip(transactionsStateUpdates)
          .map { case (id, su) => protobufStateUpdated((1, id), su) }

        val mbUpdate = protobufStateUpdated((2, microBlock.totalResBlockSig), microBlockStateUpdate)

        PBBlockchainUpdated(
          id = microBlock.totalResBlockSig,
          height = height,
          stateUpdates = Seq(mbUpdate) ++ txsUpdates,
          reason = PBBlockchainUpdated.Reason.MicroBlock(PBMicroBlocks.protobuf(microBlock)),
          `type` = PBBlockchainUpdated.UpdateType.MICROBLOCK
        )
      case RollbackCompleted(to, height) =>
        PBBlockchainUpdated(
          id = to,
          height = height,
          `type` = PBBlockchainUpdated.UpdateType.ROLLBACK
        )
      case MicroBlockRollbackCompleted(to, height) =>
        PBBlockchainUpdated(
          id = to,
          height = height,
          `type` = PBBlockchainUpdated.UpdateType.MICROBLOCK_ROLLBACK
        )
    }
}
