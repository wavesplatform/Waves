package com.wavesplatform.settings

import com.google.common.base.CaseFormat
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import com.wavesplatform.transaction.TransactionParsers

import scala.collection.JavaConverters._

case class FeeSettings(asset: String, fee: Long)
case class FeesSettings(fees: Map[Int, Seq[FeeSettings]])

object FeesSettings {
  val configPath: String = "waves.fees"

  def fromConfig(config: Config): FeesSettings = {
    val fees: Map[Int, Seq[FeeSettings]] = config
      .getObject(configPath)
      .entrySet()
      .asScala
      .flatMap { entry =>
        FeesSettings
        if (txTypes.contains(entry.getKey)) {
          val rawFees = config.as[Map[String, Long]](s"$configPath.${entry.getKey}")
          val fees    = rawFees.map { case (asset, fee) => FeeSettings(asset, fee) }(collection.breakOut)
          Some(txTypes(entry.getKey) -> fees)
        } else throw new NoSuchElementException(entry.getKey)
      }(collection.breakOut)
    FeesSettings(fees)
  }

  private def txTypes: Map[String, Int] = {
    val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_HYPHEN)
    TransactionParsers.byName
      .map {
        case (name, p) =>
          converter.convert(
            name
              .replace("V1", "")
              .replace("V2", "")
              .replace("Transaction", "")) -> p.typeId.toInt
      }
  }

}
