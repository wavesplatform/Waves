package com.wavesplatform.events.protobuf

object PBEvents {

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
      case BlockAppended(block, height, blockStateUpdate, transactionStateUpdates, txIds) =>
        val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdated)
        val txsUpdates  = transactionStateUpdates.map(protobufStateUpdated)

        PBBlockchainUpdated(
          id = block.uniqueId,
          height = height,
          update = PBBlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = txIds.map(_.toByteArray).map(PBUtils.toByteStringUnsafe),
              stateUpdate = blockUpdate,
              transactionStateUpdates = txsUpdates,
              body = PBAppend.Body.Block(PBBlocks.protobuf(block))
            )
          )
        )
      case MicroBlockAppended(microBlock, height, microBlockStateUpdate, transactionStateUpdates, txIds) =>
        val microBlockUpdate = Some(microBlockStateUpdate).filterNot(_.isEmpty).map(protobufStateUpdated)
        val txsUpdates       = transactionStateUpdates.map(protobufStateUpdated)

        PBBlockchainUpdated(
          id = microBlock.totalResBlockSig,
          height = height,
          update = PBBlockchainUpdated.Update.Append(
            PBAppend(
              transactionIds = txIds.map(_.toByteArray).map(PBUtils.toByteStringUnsafe),
              stateUpdate = microBlockUpdate,
              transactionStateUpdates = txsUpdates,
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
