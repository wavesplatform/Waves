package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._
import scala.util.Random

object NodeConfigs {

  val DefaultConfigTemplate: Config = ConfigFactory.parseResources("template.conf")

  val Default: Seq[Config] = ConfigFactory.parseResources("nodes.conf").getConfigList("nodes").asScala

  def forTest(defaultNumber: Int, special: (Int, String)): Seq[Config] = {
    val specialConfig = ConfigFactory.parseString(special._2)
    val (defaultNodes: Seq[Config], specialNodes: Seq[Config]) = Random.shuffle(Default)
      .take(defaultNumber + special._1)
      .splitAt(defaultNumber)

    defaultNodes ++ specialNodes.map(specialConfig.withFallback)
  }

}
