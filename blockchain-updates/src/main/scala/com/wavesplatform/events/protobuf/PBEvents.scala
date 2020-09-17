package com.wavesplatform.events.protobuf

import java.nio.charset.StandardCharsets

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.{
  AssetStateUpdate => VanillaAssetStateUpdate,
  StateUpdate => VanillaStateUpdate,
  BlockchainUpdated => VanillaBlockchainUpdated,
  BlockAppended => VanillaBlockAppended,
  MicroBlockAppended => VanillaMicroBlockAppended,
  RollbackCompleted => VanillaRollbackCompleted,
  MicroBlockRollbackCompleted => VanillaMicroBlockRollbackCompleted
}
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append, Rollback, Update}
import com.wavesplatform.events.protobuf.StateUpdate.AssetStateUpdate.AssetScriptInfo
import com.wavesplatform.events.protobuf.StateUpdate.{AssetStateUpdate, BalanceUpdate, DataEntryUpdate, LeasingUpdate}
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.protobuf.transaction.{PBAmounts, PBTransactions}
import com.wavesplatform.state.{LeaseBalance, AssetScriptInfo => VanillaAssetScriptInfo}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction

import scala.util.{Failure, Try}

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
        case VanillaBlockAppended(id, height, block, updatedWavesAmount, blockStateUpdate, transactionStateUpdates) =>
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
        case VanillaMicroBlockAppended(totalBlockId, height, microBlock, microBlockStateUpdate, transactionStateUpdates, totalTransactionsRoot) =>
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
    private[this] lazy val error: IllegalArgumentException = {
      val base58Id = ByteStr(self.id.toByteArray).toString
      new IllegalArgumentException(s"Invalid protobuf BlockchainUpdated at height ${self.height}, id $base58Id")
    }

    override def vanilla: Try[VanillaBlockchainUpdated] =
      Try {
        self.update match {
          case Update.Append(append) =>
            append.body match {
              case Body.Block(body) =>
                VanillaBlockAppended(
                  toId = ByteStr(self.id.toByteArray),
                  toHeight = self.height,
                  block = PBBlocks.vanilla(body.block.get, unsafe = true).get,
                  updatedWavesAmount = body.updatedWavesAmount,
                  blockStateUpdate = append.stateUpdate.map(_.vanilla.get).getOrElse(Monoid[VanillaStateUpdate].empty),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get)
                )
              case Body.MicroBlock(body) =>
                VanillaMicroBlockAppended(
                  toId = ByteStr(self.id.toByteArray),
                  toHeight = self.height,
                  microBlock = PBMicroBlocks.vanilla(body.microBlock.get, unsafe = true).get.microblock,
                  microBlockStateUpdate = append.stateUpdate.map(_.vanilla.get).getOrElse(Monoid[VanillaStateUpdate].empty),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get),
                  totalTransactionsRoot = ByteStr(body.updatedTransactionsRoot.toByteArray)
                )
              case Body.Empty => throw error
            }
          case Update.Rollback(rollback) =>
            rollback.`type` match {
              case RollbackType.BLOCK =>
                VanillaRollbackCompleted(
                  toId = ByteStr(self.id.toByteArray),
                  toHeight = self.height
                )
              case RollbackType.MICROBLOCK =>
                VanillaMicroBlockRollbackCompleted(
                  toId = ByteStr(self.id.toByteArray),
                  toHeight = self.height
                )
              case RollbackType.Unrecognized(_) => throw error
            }
          case Update.Empty => throw error
        }
      } recoverWith { case _: Throwable => Failure(error) }
  }

  implicit class AssetStateUpdateProtobuf(self: VanillaAssetStateUpdate) extends Protobuf[AssetStateUpdate] {

    import AssetStateUpdateProtobuf._

    override def protobuf: AssetStateUpdate = AssetStateUpdate(
      assetId = self.asset.id.toByteString,
      decimals = self.decimals,
      name = toStringUtf8(self.name),
      description = toStringUtf8(self.description),
      reissuable = self.reissuable,
      volume = self.volume.longValue,
      scriptInfo = self.scriptInfo.map(_.protobuf),
      sponsorship = self.sponsorship.getOrElse(0),
      nft = self.nft,
      assetExistedBefore = self.assetExistedBefore,
      safeVolume = ByteString.copyFrom(self.volume.toByteArray)
    )
  }

  object AssetStateUpdateProtobuf {
    private def toStringUtf8(bytes: ByteStr): String = new String(bytes.arr, StandardCharsets.UTF_8)
  }

  implicit class AssetStateUpdateVanilla(self: AssetStateUpdate) extends Vanilla[VanillaAssetStateUpdate] {
    import AssetStateUpdateVanilla._
    override def vanilla: Try[VanillaAssetStateUpdate] =
      Try {
        PBAmounts.toVanillaAssetId(self.assetId) match {
          case a: IssuedAsset =>
            VanillaAssetStateUpdate(
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

  implicit class AssetScriptInfoProtobuf(self: VanillaAssetScriptInfo) extends Protobuf[AssetScriptInfo] {
    override def protobuf: AssetScriptInfo = AssetScriptInfo(
      script = PBTransactions.toPBScript(Some(self.script)),
      complexity = self.complexity
    )
  }

  implicit class AssetScriptInfoVanilla(self: AssetScriptInfo) extends Vanilla[VanillaAssetScriptInfo] {
    import AssetScriptInfoVanilla._
    override def vanilla: Try[VanillaAssetScriptInfo] =
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
    import StateUpdateVanilla._
    override def vanilla: Try[VanillaStateUpdate] =
      Try {
        VanillaStateUpdate(
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
