package scorex.transaction

import scorex.crypto.encode.Base58
import scorex.settings.Settings

/**
  * Class to check, that transaction contains enough fee to put it to UTX pool
  */
class FeeCalculator(settings: Settings) {

  private val map: Map[String, Long] = settings.feeMap

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