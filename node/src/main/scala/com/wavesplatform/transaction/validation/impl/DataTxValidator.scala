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
    import tx.*

    V.seq(tx)(
      V.cond(data.length <= MaxEntryCount, TxValidationError.TooBigArray),
      V.cond(tx.data.forall(entrySizeIsValidStatic), TxValidationError.TooBigArray),
      V.cond(data.forall(_.key.nonEmpty), TxValidationError.EmptyDataKey),
      V.cond(data.map(_.key) == data.map(_.key).distinct, TxValidationError.DuplicatedDataKeys),
      V.cond(tx.version > TxVersion.V1 || tx.data.forall(!_.isEmpty), GenericError("Empty data is not allowed in V1")),
    )
  }

  private def entrySizeIsValidStatic(entry: DataEntry[?]): Boolean = {
    import DataEntry.{MaxPBKeySize, MaxValueSize}

    val keyIsValid = entry.key.utf8Bytes.length <= MaxPBKeySize

    val valueIsValid = entry match {
      case BinaryDataEntry(_, value) => value.arr.length <= MaxValueSize
      case StringDataEntry(_, value) => value.utf8Bytes.length <= MaxValueSize
      case _                         => true
    }

    keyIsValid && valueIsValid
  }

  private def entrySizeIsValid(blockchain: Blockchain, version: TxVersion)(entry: DataEntry[?]): Boolean = {
    import DataEntry.{MaxKeySize, MaxPBKeySize}

    def keyIsValid(key: String): Boolean = version match {
      case TxVersion.V1 if !blockchain.isFeatureActivated(BlockchainFeatures.RideV6) => key.length <= MaxKeySize
      case _                                                                         => key.utf8Bytes.length <= MaxPBKeySize
    }

    keyIsValid(entry.key) && entrySizeIsValidStatic(entry)
  }

  def payloadSizeValidation(blockchain: Blockchain, tx: DataTransaction): ValidatedV[DataTransaction] = {
    val fullPayloadIsValid = if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) {
      val payloadSize = realUserPayloadSize(tx.data)
      V.cond(payloadSize <= DataTransaction.MaxRideV6Bytes, TxValidationError.TooBigArray).map(_ => tx)
    } else
      V.byVersion(tx)(
        TxVersion.V1 -> { () =>
          V.cond(Try(tx.bytes().length <= DataTransaction.MaxBytes).getOrElse(false), TxValidationError.TooBigArray)
        },
        TxVersion.V2 -> { () =>
          V.cond(Try(tx.protoDataPayload.length <= DataTransaction.MaxProtoBytes).getOrElse(false), TxValidationError.TooBigArray)
        }
      )

    V.seq(tx)(
      fullPayloadIsValid,
      V.cond(tx.data.forall(entrySizeIsValid(blockchain, tx.version)), TxValidationError.TooBigArray)
    )
  }

  // For invokes
  def invokeWriteSetSize(blockchain: Blockchain, entries: Seq[DataEntry[?]]): Int =
    if (blockchain.isFeatureActivated(BlockchainFeatures.RideV6)) realUserPayloadSize(entries)
    else entries.map(_.toBytes.length).sum // Legacy behavior

  def verifyInvokeWriteSet(blockchain: Blockchain, entries: Seq[DataEntry[?]]): Either[String, Unit] = {
    val totalDataBytes = invokeWriteSetSize(blockchain, entries)
    Either.cond(
      totalDataBytes <= ContractLimits.MaxWriteSetSizeInBytes,
      (),
      s"WriteSet size can't exceed ${ContractLimits.MaxWriteSetSizeInBytes} bytes, actual: $totalDataBytes bytes"
    )
  }

  def realUserPayloadSize(entries: Seq[DataEntry[?]]): Int = {
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
