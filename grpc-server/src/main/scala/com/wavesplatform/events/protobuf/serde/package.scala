package com.wavesplatform.events.protobuf

import cats.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.StateUpdate.AssetInfo
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.{Append, Rollback, Update}
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.block.{PBBlocks, PBMicroBlocks}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.events as ve

import scala.util.{Failure, Try}

package object serde {

  implicit class BlockchainUpdatedProtobuf(val self: ve.BlockchainUpdated) extends AnyVal {

    import BlockchainUpdatedProtobuf.*

    def protobuf: BlockchainUpdated =
      self match {
        case ve.BlockAppended(
              id,
              height,
              block,
              updatedWavesAmount,
              vrf,
              activatedFeatures,
              rewardShares,
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
                    updatedWavesAmount = updatedWavesAmount,
                    activatedFeatures = activatedFeatures,
                    vrf = vrf.fold(ByteString.EMPTY)(_.toByteString),
                    rewardShares = rewardShares.map { case (addr, reward) => RewardShare(ByteString.copyFrom(addr.bytes), reward) }
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
                Some(result.stateUpdate.protobuf),
                result.deactivatedFeatures
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
    def vanillaAppend: ve.BlockAppended =
      self.update match {
        case Update.Append(append) =>
          append.body match {
            case Body.Block(body) =>
              ve.BlockAppended(
                id = self.id.toByteStr,
                height = self.height,
                block = body.block.map(PBBlocks.vanilla(_, unsafe = true).get).orNull,
                updatedWavesAmount = body.updatedWavesAmount,
                vrf = Option.unless(body.vrf.isEmpty)(body.vrf.toByteStr),
                activatedFeatures = body.activatedFeatures,
                rewardShares = body.rewardShares.map { rs => (Address.fromBytes(rs.address.toByteArray).explicitGet(), rs.reward) },
                blockStateUpdate = append.stateUpdate.fold(Monoid[ve.StateUpdate].empty)(_.vanilla.get),
                transactionStateUpdates = append.transactionStateUpdates.map(_.vanilla.get),
                transactionMetadata = append.transactionsMetadata,
                referencedAssets = self.referencedAssets.map(AssetInfo.fromPB)
              )
            case _: Body.MicroBlock => throw new IllegalArgumentException("Encountered microblock append body")
            case Body.Empty         => throw new IllegalArgumentException("Empty append body")
          }

        case _: Update.Rollback => throw new IllegalArgumentException("Encountered rollback")
        case Update.Empty       => throw new IllegalArgumentException("Update body is empty")
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
