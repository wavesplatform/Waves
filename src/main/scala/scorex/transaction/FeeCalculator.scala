package scorex.transaction

import com.wavesplatform.settings.FeesSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.transaction.FeeCalculator._
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings, state: SnapshotStateReader) {

  private val Kb = 1024

  private val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.map { v =>
        val maybeAsset = if (v.asset.toUpperCase == "WAVES") None else ByteStr.decodeBase58(v.asset).toOption
        val fee        = v.fee

        TransactionAssetFee(transactionType, maybeAsset).key -> fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, Unit] = {
    def minFeeFor(tx: T, txFeeAssetId: Option[AssetId], txMinBaseFee: Long): Long = tx match {
      case tx: DataTransaction =>
        val sizeInKb = 1 + (tx.bytes().length - 1) / Kb
        txMinBaseFee * sizeInKb
      case tx: MassTransferTransaction =>
        val transferFeeSpec = map.getOrElse(
          TransactionAssetFee(TransferTransaction.typeId, txFeeAssetId).key,
          throw new IllegalStateException("Can't find spec for TransferTransaction")
        )
        transferFeeSpec + txMinBaseFee * tx.transfers.size
      case _ => txMinBaseFee
    }

    val (txFeeAssetId, txFeeValue) = tx.assetFee
    val txAssetFeeKey              = TransactionAssetFee(tx.builder.typeId, txFeeAssetId).key
    for {
      txMinBaseFee <- Either.cond(map.contains(txAssetFeeKey), map(txAssetFeeKey), GenericError(s"Minimum fee is not defined for $txAssetFeeKey"))
      script       <- Right(tx.processingScript(state))
      _ <- Either.cond(script.isEmpty || txFeeAssetId.isEmpty,
                       (),
                       ValidationError.GenericError("Scripted accounts can accept transactions with Waves as fee only"))
      minTxFee = minFeeFor(tx, txFeeAssetId, txMinBaseFee)
      _ <- Either.cond(
        txFeeValue >= minTxFee,
        (),
        GenericError {
          s"Fee in ${txFeeAssetId.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minTxFee"
        }
      )
      totalRequiredFee = minTxFee + script.fold(0L)(_.extraFee(settings.smartAccountExtraChargePerOp, state))
      _ <- Either.cond(
        txFeeValue >= totalRequiredFee,
        (),
        GenericError(s"Scripted account requires $totalRequiredFee fee for this transaction, but given: $txFeeValue")
      )
    } yield ()
  }
}

object FeeCalculator {
  private case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {
    val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"
  }
}
