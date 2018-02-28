package com.wavesplatform.settings

import com.google.common.base.CaseFormat
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import scorex.transaction.TransactionParser.TransactionType

import scala.collection.JavaConverters._

case class FeeSettings(asset: String, fee: Long)

case class FeesSettings(fees: Map[Int, List[FeeSettings]])

object FeesSettings {
  val configPath: String = "waves.fees"

  private val converter = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)

  def fromConfig(config: Config): FeesSettings = {
    val feesEntries = config.entrySet().asScala.filter(_.getKey startsWith configPath)
    val fees = feesEntries.foldLeft(Map[Int, List[FeeSettings]]()) { (map, e) =>
      val (transactionType, asset) = extractTransactionTypeAndAsset(e.getKey)
      val maybeFee = config.getAs[Long](e.getKey)
      maybeFee.map(v => map.updated(transactionType.id, map.getOrElse(transactionType.id, List()) :+ FeeSettings(asset, v))).getOrElse(map)
    }
    FeesSettings(fees)
  }

  private def toTxType(key: String): TransactionType.Value =
    TransactionType.withName(s"${converter.convert(key)}Transaction")

  private def extractTransactionTypeAndAsset(key: String): (TransactionType.Value, String) = {
    val s = key.replace(s"$configPath.", "").trim
    val parts = s.split("\\.", 2)
    val (transactionTypeName, asset) = (parts(0), parts(1))
    (toTxType(transactionTypeName), asset)
  }
}
