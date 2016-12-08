package scorex.transaction

import play.api.libs.json.JsObject
import scorex.crypto.encode.Base58

import scala.concurrent.duration._

trait TransactionSettings {
  val settingsJSON: JsObject

  lazy val history = (settingsJSON \ "history").asOpt[String].getOrElse(DefaultHistory)

  lazy val utxRebroadcastInterval: FiniteDuration = (settingsJSON \ "utxRebroadcastInterval").asOpt[Int]
    .map(x => x.seconds).getOrElse(DefaultUtxRebroadcastInterval)

  lazy val feeMap: Map[String, Long] = (settingsJSON \ "feeMap").validate[Map[String, Map[String, Long]]].map(_.flatMap { e =>
    e._2.map { kv =>
      val assetId: Option[AssetId] = if (kv._1 == "Waves") None else Some(Base58.decode(kv._1).get)
      TransactionAssetFee(e._1.toInt, assetId).key -> kv._2
    }
  }).getOrElse(DefaultFeeMap)

  private val DefaultFeeMap: Map[String, Long] = Map(
    TransactionAssetFee(2, None).key -> 100000,
    TransactionAssetFee(3, None).key -> 100000,
    TransactionAssetFee(4, None).key -> 100000,
    TransactionAssetFee(5, None).key -> 100000,
    TransactionAssetFee(6, None).key -> 100000,
    TransactionAssetFee(7, None).key -> 100000
  )


  private val DefaultHistory = "blockchain"
  private val DefaultUtxRebroadcastInterval: FiniteDuration = 30.seconds

}
