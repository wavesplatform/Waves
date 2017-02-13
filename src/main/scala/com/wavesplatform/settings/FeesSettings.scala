package com.wavesplatform.settings

import com.typesafe.config.Config
import net.ceedubs.ficus.Ficus._
import scala.collection.JavaConverters._

case class FeeSettings(transactionType: Int, asset: String, fee: Long)

case class FeesSettings(fees: List[FeeSettings])

object FeesSettings {
  val configPath: String = "waves.fees"

  def fromConfig(config: Config): FeesSettings = {
    val feesConfigs = config.getConfigList(configPath).asScala.map { p: Config =>
      FeeSettings(p.as[Int]("transaction-type"), p.as[String]("asset"), p.as[Long]("fee"))
    }.toList

    FeesSettings(feesConfigs)
  }
}