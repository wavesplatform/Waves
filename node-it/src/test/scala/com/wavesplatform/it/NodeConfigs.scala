package com.wavesplatform.it

import com.typesafe.config.{Config, ConfigFactory}

import scala.jdk.CollectionConverters._
import scala.util.Random

object NodeConfigs {

  private val NonConflictingNodes = Set(1, 4, 6, 7)

  val Default: Seq[Config] = ConfigFactory.parseResources("nodes.conf").getConfigList("nodes").asScala.toSeq
  val Miners: Seq[Config]  = Default.init
  val NotMiner: Config     = Default.last
  def randomMiner: Config  = Random.shuffle(Miners).head

  def newBuilder: Builder = Builder(Default, Default.size, Seq.empty)

  case class Builder(baseConfigs: Seq[Config], defaultEntities: Int, specialsConfigs: Seq[Config]) {
    def overrideBase(f: Templates.type => String): Builder = {
      val priorityConfig = ConfigFactory.parseString(f(Templates))
      copy(baseConfigs = this.baseConfigs.map(priorityConfig.withFallback))
    }

    def withDefault(entitiesNumber: Int): Builder = copy(defaultEntities = entitiesNumber)

    def withSpecial(f: Templates.type => String): Builder = withSpecial(1, f)

    def withSpecial(entitiesNumber: Int, f: Templates.type => String): Builder = {
      val newSpecialConfig = ConfigFactory.parseString(f(Templates))
      copy(specialsConfigs = this.specialsConfigs ++ (1 to entitiesNumber).map(_ => newSpecialConfig))
    }

    def build(shuffleNodes: Boolean = true): Seq[Config] = {
      val totalEntities = defaultEntities + specialsConfigs.size
      require(totalEntities < baseConfigs.size)

      val baseConfigsShuffled = if (shuffleNodes) Random.shuffle(baseConfigs) else baseConfigs
      val (defaultNodes: Seq[Config], specialNodes: Seq[Config]) = baseConfigsShuffled
        .take(totalEntities)
        .splitAt(defaultEntities)

      specialNodes
        .zip(specialsConfigs)
        .foldLeft(defaultNodes) { case (r, (base, special)) => r :+ special.withFallback(base) }
    }

    // To eliminate a race of miners
    def buildNonConflicting(): Seq[Config] = {
      val totalEntities = defaultEntities + specialsConfigs.size
      require(totalEntities <= NonConflictingNodes.size)

      val (defaultNodes: Seq[Config], specialNodes: Seq[Config]) = baseConfigs.zipWithIndex
        .collect { case (x, i) if NonConflictingNodes.contains(i + 1) => x }
        .splitAt(defaultEntities)

      specialNodes
        .zip(specialsConfigs)
        .foldLeft(defaultNodes) { case (r, (base, special)) => r :+ special.withFallback(base) }
    }
  }

  object Templates {
    def raw(x: String): String = x
    def quorum(n: Int): String = s"waves.miner.quorum = $n"
    def preactivatedFeatures(f: (Int, Int)*): String = {
      s"""
         |waves.blockchain.custom.functionality.pre-activated-features {
         ${f.map { case (id, height) => s"|  $id = $height" }.mkString("\n")}
         |}""".stripMargin
    }
    def minAssetInfoUpdateInterval(blocks: Int): String =
      s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $blocks"

    val nonMiner: String = "waves.miner.enable = no"

    val lightNode: String = "waves.enable-light-mode = true"
  }

}
