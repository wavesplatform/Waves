package com.wavesplatform.events.protobuf

import cats.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append, Rollback, Update}
import com.wavesplatform.events.protobuf.StateUpdate.AssetStateUpdate.AssetScriptInfo
import com.wavesplatform.events.protobuf.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeasingUpdate}
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions}
import com.wavesplatform.state.{LeaseBalance, AssetScriptInfo => VanillaAssetScriptInfo}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.{events => ve}

import scala.util.{Failure, Try}

package object serde {

  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  implicit class BlockchainUpdatedProtobuf(val self: ve.BlockchainUpdated) extends AnyVal {

    import BlockchainUpdatedProtobuf._

    def protobuf: BlockchainUpdated =
      self match {
        case ve.BlockAppended(id, height, block, updatedWavesAmount, blockStateUpdate, transactionStateUpdates) =>
          val blockUpdate = Some(blockStateUpdate).filterNot(_.isEmpty).map(_.protobuf)
          val txsUpdates  = transactionStateUpdates.map(_.protobuf)

          BlockchainUpdated(
            id = id.toByteString,
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
        case ve.MicroBlockAppended(totalBlockId, height, microBlock, microBlockStateUpdate, transactionStateUpdates, totalTransactionsRoot) =>
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
                    microBlock = Some(PBMicroBlocks.protobuf(microBlock, totalBlockId)),
                    updatedTransactionsRoot = totalTransactionsRoot.toByteString
                  )
                )
              )
            )
          )
        case ve.RollbackCompleted(to, height) =>
          BlockchainUpdated(
            id = to.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Rollback(
              Rollback(Rollback.RollbackType.BLOCK)
            )
          )
        case ve.MicroBlockRollbackCompleted(toSig, height) =>
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

  implicit class BlockchainUpdatedVanilla(val self: BlockchainUpdated) extends AnyVal {
    private def throwError() = {
      val base58Id = self.id.toByteStr.toString
      throw new IllegalArgumentException(s"Invalid protobuf BlockchainUpdated at height ${self.height}, id $base58Id")
    }

    def vanilla: Try[ve.BlockchainUpdated] =
      Try {
        self.update match {
          case Update.Append(append) =>
            append.body match {
              case Body.Block(body) =>
                ve.BlockAppended(
                  toId = self.id.toByteStr,
                  toHeight = self.height,
                  block = PBBlocks.vanilla(body.block.get, unsafe = true).get,
                  updatedWavesAmount = body.updatedWavesAmount,
                  blockStateUpdate = append.stateUpdate.fold(Monoid[ve.StateUpdate].empty)(_.vanilla.get),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get)
                )
              case Body.MicroBlock(body) =>
                ve.MicroBlockAppended(
                  toId = self.id.toByteStr,
                  toHeight = self.height,
                  microBlock = PBMicroBlocks.vanilla(body.microBlock.get, unsafe = true).get.microblock,
                  microBlockStateUpdate = append.stateUpdate.fold(Monoid[ve.StateUpdate].empty)(_.vanilla.get),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get),
                  totalTransactionsRoot = body.updatedTransactionsRoot.toByteStr
                )
              case Body.Empty => throwError()
            }
          case Update.Rollback(rollback) =>
            rollback.`type` match {
              case RollbackType.BLOCK =>
                ve.RollbackCompleted(
                  toId = self.id.toByteStr,
                  toHeight = self.height
                )
              case RollbackType.MICROBLOCK =>
                ve.MicroBlockRollbackCompleted(
                  toId = self.id.toByteStr,
                  toHeight = self.height
                )
              case RollbackType.Unrecognized(_) => throwError()
            }
          case Update.Empty => throwError()
        }
      } recoverWith { case _: Throwable => Try(throwError()) }
  }

  implicit class AssetStateUpdateProtobuf(val self: ve.AssetStateUpdate) extends AnyVal {
    def protobuf: AssetStateUpdate = AssetStateUpdate(
      assetId = self.asset.id.toByteString,
      decimals = self.decimals,
      name = self.name.toUTF8String,
      description = self.description.toUTF8String,
      reissuable = self.reissuable,
      volume = self.volume.longValue,
      scriptInfo = self.scriptInfo.map(_.protobuf),
      sponsorship = self.sponsorship.getOrElse(0),
      nft = self.nft,
      assetExistedBefore = self.assetExistedBefore,
      safeVolume = ByteString.copyFrom(self.volume.toByteArray)
    )
  }

  implicit class AssetStateUpdateVanilla(val self: AssetStateUpdate) extends AnyVal {
    import AssetStateUpdateVanilla._
    def vanilla: Try[ve.AssetStateUpdate] =
      Try {
        PBAmounts.toVanillaAssetId(self.assetId) match {
          case a: IssuedAsset =>
            ve.AssetStateUpdate(
              asset = a,
              decimals = self.decimals,
              name = ByteStr(self.name.getBytes()),
              description = ByteStr(self.description.getBytes()),
              reissuable = self.reissuable,
              volume = BigInt(self.safeVolume.toByteArray),
              scriptInfo = self.scriptInfo.map(_.vanilla.get),
              sponsorship = if (self.sponsorship == 0) None else Some(self.sponsorship),
              nft = self.nft,
              assetExistedBefore = self.assetExistedBefore
            )
          case _ => throw error
        }
      } recoverWith { case _: Throwable => Failure(error) }
  }

  object AssetStateUpdateVanilla {
    private lazy val error = new IllegalArgumentException(s"Invalid protobuf AssetStateUpdate")
  }

  implicit class AssetScriptInfoProtobuf(val self: VanillaAssetScriptInfo) extends AnyVal {
    def protobuf: AssetScriptInfo = AssetScriptInfo(
      script = PBTransactions.toPBScript(Some(self.script)),
      complexity = self.complexity
    )
  }

  implicit class AssetScriptInfoVanilla(val self: AssetScriptInfo) extends AnyVal {
    import AssetScriptInfoVanilla._
    def vanilla: Try[VanillaAssetScriptInfo] =
      Try {
        VanillaAssetScriptInfo(
          script = PBTransactions.toVanillaScript(self.script).get,
          complexity = self.complexity
        )
      } recoverWith { case _: Throwable => Failure(error) }
  }

  object AssetScriptInfoVanilla {
    private lazy val error = new IllegalArgumentException(s"Invalid protobuf AssetScriptInfo")
  }

  implicit class StateUpdateProtobuf(val self: ve.StateUpdate) extends AnyVal {
    def protobuf: StateUpdate = StateUpdate(
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

  implicit class StateUpdateVanilla(val self: StateUpdate) extends AnyVal {
    import StateUpdateVanilla._
    def vanilla: Try[ve.StateUpdate] =
      Try {
        ve.StateUpdate(
          balances = self.balances.map { b =>
            val (asset, balance) = PBAmounts.toAssetAndAmount(b.amount.get)
            val address          = toAddress(b.address).get
            (address, asset, balance)
          },
          leases = self.leases.map { l =>
            val address = toAddress(l.address).get
            (address, LeaseBalance(l.in, l.out))
          },
          dataEntries = self.dataEntries.map {
            case DataEntryUpdate(addr, entry) =>
              (toAddress(addr).get, PBTransactions.toVanillaDataEntry(entry.get))
          },
          assets = self.assets.map(_.vanilla.get)
        )
      } recoverWith { case _: Throwable => Failure(error) }
  }

  object StateUpdateVanilla {
    private lazy val error = new IllegalArgumentException(s"Invalid protobuf StateUpdate")

    def toAddress(bs: ByteString): Try[Address] = Address.fromBytes(bs.toByteArray).left.map(_ => error).toTry
  }
}
