package scorex.transaction

import com.wavesplatform.settings.FeesSettings
import com.wavesplatform.state2.ByteStr
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.MassTransferTransaction

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings) {

  private val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.map { v =>
        val maybeAsset = if (v.asset.toUpperCase == "WAVES") None else ByteStr.decodeBase58(v.asset).toOption
        val fee = v.fee

        TransactionAssetFee(transactionType, maybeAsset).key -> fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = {
    map.get(TransactionAssetFee(tx.transactionType.id, tx.assetFee._1).key) match {
      case Some(minimumFee) =>
        val min = tx match {
          case _: MassTransferTransaction => minimumFee * tx.bytes().length
          case _ => minimumFee
        }
        if (min <= tx.assetFee._2) {
          Right(tx)
        } else {
          Left(GenericError(s"Fee in ${tx.assetFee._1.fold("WAVES")(_.toString)} for ${tx.transactionType} transaction does not exceed minimal value of $min"))
        }
      case None =>
        Left(GenericError(s"Minimum fee is not defined for ${TransactionAssetFee(tx.transactionType.id, tx.assetFee._1).key}"))
    }
  }
}

case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {

  val key = s"TransactionAssetFee($txType, ${assetId.map(_.base58)})"

}