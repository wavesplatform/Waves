package scorex.transaction

import com.wavesplatform.settings.FeesSettings
import scorex.crypto.encode.Base58

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: FeesSettings) {

  private val map: Map[String, Long] = {
    settings.fees.map { fs =>
      val transactionType = fs.transactionType
      val maybeAsset = if (fs.asset.toUpperCase == "WAVES") None else Base58.decode(fs.asset).toOption
      val fee = fs.fee

      TransactionAssetFee(transactionType, maybeAsset).key -> fee
    }.toMap
  }

  def enoughFee(tx: Transaction): Boolean = tx match {
    case ttx: TypedTransaction =>
      map.get(TransactionAssetFee(ttx.transactionType.id, ttx.assetFee._1).key).exists(_ <= ttx.assetFee._2)
    case _ => false
  }
}

case class TransactionAssetFee(txType: Int, assetId: Option[AssetId]) {
  override def hashCode(): Int = txType.hashCode() + assetId.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case o: TransactionAssetFee => o.key == this.key
    case e => false
  }

  val key = s"TransactionAssetFee($txType, ${assetId.map(Base58.encode)})"

  override def toString: String = key
}