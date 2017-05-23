package scorex.transaction

import com.wavesplatform.settings.FeesSettings
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.TransactionValidationError

class FeeCalculator(settings: FeesSettings) {

  private val map: Map[String, Long] = {
    settings.fees.flatMap { fs =>
      val transactionType = fs._1
      fs._2.map { v =>
        val maybeAsset = if (v.asset.toUpperCase == "WAVES") None else Base58.decode(v.asset).toOption
        val fee = v.fee

        TransactionAssetFee(transactionType, maybeAsset).key -> fee
      }
    }
  }

  def enoughFee[T <: Transaction](tx: T): Either[ValidationError, T] = tx match {
    case ttx: Transaction if map.get(TransactionAssetFee(ttx.transactionType.id, ttx.assetFee._1).key).exists(_ <= ttx.assetFee._2) => Right(tx)
    case _ => Left(TransactionValidationError(tx, "InsufficientFee: Node's settings require more fee or fee in this asset is not enabled"))
  }
}

case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {
  override def hashCode(): Int = txType.hashCode() + assetId.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case o: TransactionAssetFee => o.key == this.key
    case _ => false
  }

  val key = s"TransactionAssetFee($txType, ${assetId.map(Base58.encode)})"

  override def toString: String = key
}