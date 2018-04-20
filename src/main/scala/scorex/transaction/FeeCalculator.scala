package scorex.transaction

import com.wavesplatform.settings.{FeesSettings, FunctionalitySettings}
import com.wavesplatform.state2.reader.SnapshotStateReader
import com.wavesplatform.state2.{ByteStr, Sponsorship}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets._

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
// will be unused once we switch to sponsored fees
class FeeCalculator(settings: FeesSettings) {

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

  def enoughFee[T <: Transaction](tx: T, s: SnapshotStateReader, history: History, fs: FunctionalitySettings): Either[ValidationError, T] =
    if (history.height >= Sponsorship.sponsoredFeesSwitchHeight(history, fs)) Right(tx)
    else enoughFee(tx)

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    val feeSpec = map.get(TransactionAssetFee(tx.builder.typeId, tx.assetFee._1).key)
    val feeValue = tx match {
      case dt: DataTransaction =>
        val sizeInKb = 1 + (dt.bytes().length - 1) / Kb
        feeSpec.map(_ * sizeInKb)
      case mtt: MassTransferTransaction =>
        val transferFeeSpec = map.get(TransactionAssetFee(TransferTransaction.typeId, tx.assetFee._1).key)
        feeSpec.flatMap(mfee => transferFeeSpec.map(tfee => tfee + mfee * mtt.transfers.size))
      case _ => feeSpec
    }

    feeValue match {
      case Some(minimumFee) =>
        if (minimumFee <= tx.assetFee._2) {
          Right(tx)
        } else {
          Left(GenericError(
            s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.builder.classTag} transaction does not exceed minimal value of $minimumFee"))
        }
      case None =>
        Left(GenericError(s"Minimum fee is not defined for ${TransactionAssetFee(tx.builder.typeId, tx.assetFee._1).key}"))
    }
  }
}

case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {

  val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"

}
