package com.wavesplatform.settings

import java.util.Map.Entry

import com.typesafe.config.ConfigException.{BadValue, NotResolved}
import com.typesafe.config.{Config, ConfigException, ConfigValue}

import scala.collection.JavaConverters._
import scala.util.Try

case class FeeSettings(asset: String, fee: Long)

case class FeesSettings(fees: Map[Int, List[FeeSettings]])

object FeesSettings {
  val configPath: String = "waves.fees"

  def fromConfig(config: Config): FeesSettings = {
    val feesEntries = config.entrySet().asScala.filter(_.getKey startsWith(configPath))
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
    val transactionType = transactionTypeName match {
      case "payment" => 2
      case "issue" => 3
      case "transfer" => 4
      case "reissue" => 5
      case "burn" => 6
      case "exchange" => 7
      case "lease" => 8
      case "lease-cancel" => 9
      case _ => throw new NotResolved(s"Unsupported transaction '$transactionTypeName' in fee configuration section 'waves.fees'")
    }

    val feeString = e.getValue.render
    val triedFee = Try(feeString.toLong)

    if (triedFee.isFailure) throw new BadValue(e.getKey, s"Failed to convert $feeString to long value", triedFee.failed.get)

    transactionType -> FeeSettings(asset, triedFee.get)
  }
}