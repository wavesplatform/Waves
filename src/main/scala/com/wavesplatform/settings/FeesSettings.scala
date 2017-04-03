package com.wavesplatform.settings

import java.util.Map.Entry
import scala.collection.JavaConverters._
import scala.util.Try
import com.google.common.base.CaseFormat
import com.typesafe.config.ConfigException.BadValue
import com.typesafe.config.{Config, ConfigValue}
import scorex.transaction.TransactionParser.TransactionType

case class FeeSettings(asset: String, fee: Long)

case class FeesSettings(fees: Map[Int, List[FeeSettings]])

object FeesSettings {
  val configPath: String = "waves.fees"

  private val converter = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
  private def toTxType(key: String): TransactionType.Value =
    TransactionType.withName(s"${converter.convert(key)}Transaction")

  def fromConfig(config: Config): FeesSettings = {
    val feesEntries = config.entrySet().asScala.filter(_.getKey startsWith configPath)
    val fees = feesEntries.foldLeft(Map[Int, List[FeeSettings]]()) { (map, e) =>
      val p = toFeeSettings(e)
      map.updated(p._1, map.getOrElse(p._1, List()) :+ p._2)
    }

    FeesSettings(fees)
  }

  private def toFeeSettings(e: Entry[String, ConfigValue]): (Int, FeeSettings) = {
    val s = e.getKey.replace(s"$configPath.", "").trim
    val parts = s.split("\\.", 2)
    val (transactionTypeName, asset) = (parts(0), parts(1))
    val transactionType = toTxType(transactionTypeName).id

    val feeString = e.getValue.render
    val triedFee = Try(feeString.toLong)

    if (triedFee.isFailure) throw new BadValue(e.getKey, s"Failed to convert $feeString to long value", triedFee.failed.get)

    transactionType -> FeeSettings(asset, triedFee.get)
  }
}
