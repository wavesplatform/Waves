package com.wavesplatform.settings

import com.google.common.base.CaseFormat
import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import scorex.transaction.TransactionParser.TransactionType

case class FeeSettings(asset: String, fee: Long)

case class FeesSettings(fees: Map[Int, Seq[FeeSettings]])

object FeesSettings {
  val configPath: String = "waves.fees"

  private val converter = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)

  def fromConfig(config: Config): FeesSettings =
    FeesSettings(for {
      (txTypeName, fs) <- config.as[Map[String, Map[String, Long]]](configPath)
      fees = fs.map { case (asset, fee) => FeeSettings(asset, fee) }.toSeq
    } yield toTxType(txTypeName) -> fees)

  private def toTxType(key: String): Int =
    TransactionType.withName(s"${converter.convert(key)}Transaction").id
}
