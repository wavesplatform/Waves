package com.wavesplatform.settings

import com.typesafe.config.Config

import scala.concurrent.duration.FiniteDuration

import net.ceedubs.ficus.Ficus._

case class UTXSettings(size: Int, broadcastInterval: FiniteDuration)

object UTXSettings {
  val configPath: String = "waves.utx"

  def fromConfig(config: Config): UTXSettings = {
    val size = config.as[Int](s"$configPath.size")
    val broadcastInterval = config.as[FiniteDuration](s"$configPath.broadcast-interval")

    UTXSettings(size, broadcastInterval)
  }
}