package com.wavesplatform.events.protobuf

import cats.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.events.RollbackResult
import com.wavesplatform.events.StateUpdate.AssetInfo
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Rollback.RollbackType
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append, Rollback, Update}
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.{events => ve}

import scala.util.{Failure, Try}

package object serde {

  implicit class BlockchainUpdatedProtobuf(val self: ve.BlockchainUpdated) extends AnyVal {

    import BlockchainUpdatedProtobuf._

    def protobuf: BlockchainUpdated =
      self match {
        case ve.BlockAppended(
            id,
            height,
            block,
            updatedWavesAmount,
            blockStateUpdate,
            transactionStateUpdates,
            transactionsMetadata,
            referencedAssets
            ) =>
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
                transactionsMetadata = transactionsMetadata,
                body = Append.Body.Block(
                  Append.BlockAppend(
                    block = Some(PBBlocks.protobuf(block)),
                    updatedWavesAmount = updatedWavesAmount
                  )
                )
              )
            ),
            referencedAssets = referencedAssets.map(AssetInfo.toPB)
          )

        case ve.MicroBlockAppended(
            totalBlockId,
            height,
            microBlock,
            microBlockStateUpdate,
            transactionStateUpdates,
            transactionsMetadata,
            totalTransactionsRoot,
            referencedAssets
            ) =>
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
                transactionsMetadata = transactionsMetadata,
                body = Append.Body.MicroBlock(
                  Append.MicroBlockAppend(
                    microBlock = Some(PBMicroBlocks.protobuf(microBlock, totalBlockId)),
                    updatedTransactionsRoot = totalTransactionsRoot.toByteString
                  )
                )
              )
            ),
            referencedAssets = referencedAssets.map(AssetInfo.toPB)
          )
        case ve.RollbackCompleted(to, height, result, referencedAssets) =>
          BlockchainUpdated(
            id = to.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Rollback(
              Rollback(
                Rollback.RollbackType.BLOCK,
                result.removedTransactionIds.map(_.toByteString),
                result.removedBlocks.map(PBBlocks.protobuf),
                Some(result.stateUpdate.protobuf)
              )
            ),
            referencedAssets = referencedAssets.map(AssetInfo.toPB)
          )
        case ve.MicroBlockRollbackCompleted(toSig, height, result, referencedAssets) =>
          BlockchainUpdated(
            id = toSig.toByteString,
            height = height,
            update = BlockchainUpdated.Update.Rollback(
              Rollback(
                Rollback.RollbackType.MICROBLOCK,
                result.removedTransactionIds.map(_.toByteString),
                Nil,
                Some(result.stateUpdate.protobuf)
              )
            ),
            referencedAssets = referencedAssets.map(AssetInfo.toPB)
          )
      }
  }

  object BlockchainUpdatedProtobuf {
    private def getIds(txs: Seq[Transaction]): Seq[ByteString] = txs.map(t => ByteString.copyFrom(t.id().arr))
  }

  implicit class BlockchainUpdatedVanilla(val self: BlockchainUpdated) extends AnyVal {
    def vanilla: Try[ve.BlockchainUpdated] =
      Try {
        self.update match {
          case Update.Append(append) =>
            append.body match {
              case Body.Block(body) =>
                ve.BlockAppended(
                  id = self.id.toByteStr,
                  height = self.height,
                  block = body.block.map(PBBlocks.vanilla(_, unsafe = true).get).orNull,
                  updatedWavesAmount = body.updatedWavesAmount,
                  blockStateUpdate = append.stateUpdate.fold(Monoid[ve.StateUpdate].empty)(_.vanilla.get),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get),
                  transactionMetadata = append.transactionsMetadata,
                  referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
                )
              case Body.MicroBlock(body) =>
                ve.MicroBlockAppended(
                  id = self.id.toByteStr,
                  height = self.height,
                  microBlock = PBMicroBlocks.vanilla(body.microBlock.get, unsafe = true).get.microblock,
                  microBlockStateUpdate = append.stateUpdate.fold(Monoid[ve.StateUpdate].empty)(_.vanilla.get),
                  transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get),
                  totalTransactionsRoot = body.updatedTransactionsRoot.toByteStr,
                  transactionMetadata = append.transactionsMetadata,
                  referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
                )
              case Body.Empty => throw new IllegalArgumentException("Empty append body")
            }

          case Update.Rollback(rollback) =>
            rollback.`type` match {
              case RollbackType.BLOCK =>
                ve.RollbackCompleted(
                  self.id.toByteStr,
                  self.height,
                  RollbackResult(
                    rollback.removedBlocks.map(PBBlocks.vanilla(_).get),
                    rollback.removedTransactionIds.map(_.toByteStr),
                    rollback.getRollbackStateUpdate.vanilla.get
                  ),
                  referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
                )
              case RollbackType.MICROBLOCK =>
                ve.MicroBlockRollbackCompleted(
                  id = self.id.toByteStr,
                  height = self.height,
                  RollbackResult.micro(rollback.removedTransactionIds.map(_.toByteStr), rollback.getRollbackStateUpdate.vanilla.get),
                  referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
                )
              case RollbackType.Unrecognized(v) => throw new IllegalArgumentException(s"Unrecognized rollback type $v")
            }
          case Update.Empty => throw new IllegalArgumentException("Update body is empty")
        }
      } recoverWith {
        case err: Throwable =>
          Failure(new IllegalArgumentException(s"Invalid protobuf BlockchainUpdated at height ${self.height}, id ${self.id.toByteStr}", err))
      }
  }

  implicit class StateUpdateVanilla(val self: StateUpdate) extends AnyVal {
    def vanilla: Try[ve.StateUpdate] =
      Try {
        ve.StateUpdate.fromPB(self)
      } recoverWith { case err: Throwable => Failure(new IllegalArgumentException(s"Invalid protobuf StateUpdate", err)) }
  }

  implicit class StateUpdateProtobuf(val self: ve.StateUpdate) extends AnyVal {
    def protobuf: StateUpdate = ve.StateUpdate.toPB(self)
  }
}
