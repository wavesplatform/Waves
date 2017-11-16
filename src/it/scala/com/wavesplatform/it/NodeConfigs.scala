package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}

import scala.collection.JavaConverters._
import scala.util.Random

object NodeConfigs {

  val DefaultConfigTemplate: Config = ConfigFactory.parseResources("template.conf")

  val Default: Seq[Config] = ConfigFactory.parseResources("nodes.conf").getConfigList("nodes").asScala

  def newBuilder: Builder = Builder(Default, Default.size, Seq.empty)

  case class Builder(baseConfigs: Seq[Config],
                     defaultEntities: Int,
                     specialsConfigs: Seq[Config]) {
    def overrideBase(f: Templates.type => String): Builder = {
      val priorityConfig = ConfigFactory.parseString(f(Templates))
      copy(baseConfigs = baseConfigs.map(priorityConfig.withFallback))
    }

    def withDefault(entitiesNumber: Int): Builder = copy(defaultEntities = entitiesNumber)

    def withSpecial(f: Templates.type => String): Builder = {
      copy(specialsConfigs = specialsConfigs :+ ConfigFactory.parseString(f(Templates)))
    }

    def build: Seq[Config] = {
      val totalEntities = defaultEntities + specialsConfigs.size
      require(totalEntities < baseConfigs.size)

      val (defaultNodes: Seq[Config], specialNodes: Seq[Config]) = Random.shuffle(baseConfigs)
        .take(totalEntities)
        .splitAt(defaultEntities)

      specialNodes.zip(specialsConfigs)
        .foldLeft(defaultNodes) { case (r, (base, special)) => r :+ special.withFallback(base) }
    }
  }

  object Templates {
    def quorum(n: Int): String = s"waves.miner.quorum = $n"
    val nonMiner: String = "waves.miner.enable = no"
  }

}
