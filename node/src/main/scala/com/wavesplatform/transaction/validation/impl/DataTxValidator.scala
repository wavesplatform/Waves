package com.wavesplatform.transaction.validation.impl

import scala.util.Try

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, BooleanDataEntry, DataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.{DataTransaction, TxValidationError, TxVersion}
import com.wavesplatform.transaction.DataTransaction.MaxEntryCount
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.validation.{TxValidator, ValidatedV}
import com.wavesplatform.utils.StringBytes

object DataTxValidator extends TxValidator[DataTransaction] {
  override def validate(tx: DataTransaction): ValidatedV[DataTransaction] = {
    import tx._

    V.seq(tx)(
      V.cond(data.length <= MaxEntryCount && data.forall(_.isValid(version)), TxValidationError.TooBigArray),
      V.cond(data.forall(_.key.nonEmpty), TxValidationError.EmptyDataKey),
      V.cond(data.map(_.key) == data.map(_.key).distinct, TxValidationError.DuplicatedDataKeys),
      V.fee(fee)
    )
  }

  def entrySizeValidation(blockchain: Blockchain, tx: DataTransaction): ValidatedV[DataTransaction] = {
    if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) {
      val payloadSize = realUserPayloadSize(tx.data)
      V.cond(payloadSize <= DataTransaction.MaxBytes, TxValidationError.TooBigArray).map(_ => tx)
    } else
      V.byVersion(tx)(
        TxVersion.V1 -> { () =>
          V.seq(tx)(
            V.cond(tx.data.forall(!_.isEmpty), GenericError("Empty data is not allowed in V1")),
            V.cond(Try(tx.bytes().length <= DataTransaction.MaxBytes).getOrElse(false), TxValidationError.TooBigArray)
          )
        },
        TxVersion.V2 -> { () =>
          V.cond(Try(tx.protoDataPayload.length <= DataTransaction.MaxProtoBytes).getOrElse(false), TxValidationError.TooBigArray)
        }
      )
  }

  // For invokes
  def invokeWriteSetSize(blockchain: Blockchain, entries: Seq[DataEntry[_]]): Int =
    if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) realUserPayloadSize(entries)
    else entries.map(_.toBytes.length).sum // Legacy behavior

  def verifyInvokeWriteSet(blockchain: Blockchain, entries: Seq[DataEntry[_]]): Either[String, Unit] = {
    val totalDataBytes = invokeWriteSetSize(blockchain, entries)
    Either.cond(
      totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes,
      (),
      s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"
    )
  }

  private[this] def realUserPayloadSize(entries: Seq[DataEntry[_]]): Int = {
    entries
      .flatMap(
        e =>
          Iterable(
            e.key.utf8Bytes.length,
            e match {
              case EmptyDataEntry(_)         => 0 // Delete
              case BooleanDataEntry(_, _)    => 1
              case IntegerDataEntry(_, _)    => 8
              case BinaryDataEntry(_, value) => value.size
              case StringDataEntry(_, value) => value.utf8Bytes.length
            }
          )
      )
      .fold(0)(Math.addExact)
  }
}
