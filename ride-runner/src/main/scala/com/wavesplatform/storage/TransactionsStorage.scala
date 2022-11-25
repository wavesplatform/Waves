package com.wavesplatform.storage

import cats.syntax.option.*
import com.google.protobuf.ByteString
import com.wavesplatform.api.grpc.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction
import com.wavesplatform.state.{Height, TransactionId, TxMeta}
import com.wavesplatform.storage.actions.AppendResult
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, EthereumTransaction, Inspect, Transaction, TxPositiveAmount}

class TransactionsStorage[TagT](
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[TransactionId, (TxMeta, Option[Transaction])]
) extends HeightStorage[TransactionId, (TxMeta, Option[Transaction]), TagT] {
  override def getFromBlockchain(key: TransactionId): Option[(TxMeta, Option[Transaction])] = blockchainApi.getTransferLikeTransaction(key)

  def getWithTransferLike(height: Int, key: TransactionId, tag: TagT): Option[(TxMeta, Option[TransferTransactionLike])] =
    get(height, key, tag).map { case (meta, maybeTx) =>
      val tx = maybeTx.flatMap {
        case tx: TransferTransactionLike => tx.some
        case tx: EthereumTransaction =>
          tx.payload match {
            case transfer: EthereumTransaction.Transfer =>
              tx
                .toTransferLike(
                  TxPositiveAmount.from(transfer.amount).explicitGet(),
                  transfer.recipient,
                  // TODO have to call GET /eth/assets/... or blockchain.resolveERC20Address
                  transfer.tokenAddress.fold(Asset.Waves)(x => throw new RuntimeException(s"unknown asset: $x"))
                )
                .some
            case _ => none
          }
        case _ => none
      }
      (meta, tx)
    }

  // Got a transaction, got a rollback, same transaction on new height/failed/removed
  def append(height: Int, pbTxId: ByteString, signedTx: SignedTransaction): AppendResult[TagT] = {
    val txId = TransactionId(pbTxId.toByteStr)
    super.append(
      height,
      txId,
      (
        TxMeta(
          height = Height(height),
          succeeded = true,
          spentComplexity = 0
        ),
        signedTx.toVanilla.toOption.flatMap { tx =>
          if (Inspect.isTransferLike(tx)) tx.some
          else none
        }
      ).some
    )
  }
}
