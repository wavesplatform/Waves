package com.wavesplatform.events.protobuf

import java.nio.charset.StandardCharsets

import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.{
  AssetStateUpdate => VanillaAssetStateUpdate,
  StateUpdate => VanillaStateUpdate,
  BlockAppended => VanillaBlockAppended,
  MicroBlockAppended => VanilllaMicroBlockAppended,
  MicroBlockRollbackCompleted => VanillaMicroBlockRollbackCompleted,
  RollbackCompleted => VanillaRollbackCompleted,
  BlockchainUpdated => VanillaBlockchainUpdated
}
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Update, Append, Rollback}
import com.wavesplatform.events.protobuf.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeasingUpdate}
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.Transaction

import scala.util.{Failure, Success, Try}

trait Protobuf[A] {
  def protobuf: A
}

trait Vanilla[A] {
  def vanilla: Try[A]
}

object serde {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  implicit class BlockchainUpdatedProtobuf(self: VanillaBlockchainUpdated) extends Protobuf[BlockchainUpdated] {
    import BlockchainUpdatedProtobuf._

    override def protobuf: BlockchainUpdated =
      self match {
        case VanillaBlockAppended(sig, height, block, updatedWavesAmount, blockStateUpdate, transactionStateUpdates) =>
          val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(_.protobuf)
          val txsUpdates  = transactionStateUpdates.map(_.protobuf)

          BlockchainUpdated(
            id = sig.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Append(
              Append(
                transactionIds = getIds(block.transactionData),
                stateUpdate = blockUpdate,
                transactionStateUpdates = txsUpdates,
                body = Append.Body.Block(
                  Append.BlockAppend(
                    block = Some(PBBlocks.protobuf(block)),
                    updatedWavesAmount = updatedWavesAmount
                  )
                )
              )
            )
          )
        case VanilllaMicroBlockAppended(totalBlockId, height, microBlock, microBlockStateUpdate, transactionStateUpdates) =>
          val microBlockUpdate = Some(microBlockStateUpdate).filterNot(_.isEmpty).map(_.protobuf)
          val txsUpdates       = transactionStateUpdates.map(_.protobuf)

          BlockchainUpdated(
            id = totalBlockId.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Append(
              Append(
                transactionIds = getIds(microBlock.transactionData),
                stateUpdate = microBlockUpdate,
                transactionStateUpdates = txsUpdates,
                body = Append.Body.MicroBlock(
                  Append.MicroBlockAppend(
                    microBlock = Some(PBMicroBlocks.protobuf(microBlock, totalBlockId))
                  )
                )
              )
            )
          )
        case VanillaRollbackCompleted(to, height) =>
          BlockchainUpdated(
            id = to.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Rollback(
              Rollback(Rollback.RollbackType.BLOCK)
            )
          )
        case VanillaMicroBlockRollbackCompleted(toSig, height) =>
          BlockchainUpdated(
            id = toSig.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Rollback(
              Rollback(Rollback.RollbackType.MICROBLOCK)
            )
          )
      }
  }

  object BlockchainUpdatedProtobuf {
    private def getIds(txs: Seq[Transaction]): Seq[ByteString] = txs.map(t => ByteString.copyFrom(t.id().arr))
  }

  implicit class BlockchainUpdatedVanilla(self: BlockchainUpdated) extends Vanilla[VanillaBlockchainUpdated] {
    private[this] lazy val failure: Failure[IllegalArgumentException] = {
      val base58Id = ByteStr(self.id.toByteArray).toString
      Failure(new IllegalArgumentException(s"Invalid protobuf BlockchainUpdated at height ${self.height}, id $base58Id"))
    }

    // todo finish
    override def vanilla: Try[VanillaBlockchainUpdated] = ???
//      try {
//        self.update match {
//          case Update.Append(append) =>
//            append.body match {
//              case Body.Block(body) =>
//                Success {
//                  events.BlockAppended(
//                    toId = ByteStr(self.id.toByteArray),
//                    toHeight = self.height,
//                    // unsafe
//                    block = PBBlocks.vanilla(body.block.get, unsafe = false).get,
//                    updatedWavesAmount = body.updatedWavesAmount,
//                    blockStateUpdate = append.stateUpdate
//                  )
//                }
//
//              case Body.Empty => failure
//            }
//          //      case Update.Rollback(value) =>
//          case Update.Empty => failure
//        }
//      } catch {
//        case t: Throwable => failure
//      }
  }

  implicit class AssetStateUpdateProtobufImpl(self: VanillaAssetStateUpdate) extends Protobuf[AssetStateUpdate] {
    import AssetStateUpdateProtobufImpl._

    override def protobuf: AssetStateUpdate = AssetStateUpdate(
      assetId = self.asset.id.toByteString,
      decimals = self.decimals,
      name = toStringUtf8(self.name),
      description = toStringUtf8(self.description),
      reissuable = self.reissuable,
      volume = self.volume.longValue,
      script = PBTransactions.toPBScript(self.script.map(_.script)),
      sponsorship = self.sponsorship.getOrElse(0),
      nft = self.nft,
      assetExistedBefore = self.assetExistedBefore,
      safeVolume = ByteString.copyFrom(self.volume.toByteArray)
    )
  }

  object AssetStateUpdateProtobufImpl {
    private def toStringUtf8(bytes: ByteStr): String = new String(bytes.arr, StandardCharsets.UTF_8)
  }

  implicit class StateUpdateProtobuf(self: VanillaStateUpdate) extends Protobuf[StateUpdate] {
    override def protobuf: StateUpdate = StateUpdate(
      balances = self.balances.map {
        case (addr, assetId, amt) =>
          BalanceUpdate(address = addr, amount = Some((assetId, amt)))
      },
      leases = self.leases.map {
        case (addr, leaseBalance) =>
          LeasingUpdate(address = addr, in = leaseBalance.in, out = leaseBalance.out)
      },
      dataEntries = self.dataEntries.map {
        case (addr, entry) => DataEntryUpdate(address = addr, dataEntry = Some(PBTransactions.toPBDataEntry(entry)))
      },
      assets = self.assets.map(_.protobuf)
    )
  }

  implicit class StateUpdateVanilla(self: StateUpdate) extends Vanilla[VanillaStateUpdate] {
    override def vanilla: Try[VanillaStateUpdate] = ???
//    events.StateUpdate(
//      balances = su.balances.map {
//        case (addr, assetId, amt) =>
//          PBBalanceUpdate(address = addr, amount = Some((assetId, amt)))
//      },
//      leases = su.leases.map {
//        case (addr, leaseBalance) =>
//          PBLeasingUpdate(address = addr, in = leaseBalance.in, out = leaseBalance.out)
//      },
//      dataEntries = su.dataEntries.map {
//        case (addr, entry) => PBDataEntryUpdate(address = addr, dataEntry = Some(PBTransactions.toPBDataEntry(entry)))
//      },
//      assets = su.assets.map(protobufAssetStateUpdate)
//    )
  }
}
