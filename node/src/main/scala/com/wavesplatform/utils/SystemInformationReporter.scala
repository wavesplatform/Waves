package com.wavesplatform.utils

import com.typesafe.config.{Config, ConfigFactory, ConfigRenderOptions}

object SystemInformationReporter extends ScorexLogging {
  def configForLogs(config: Config): Config = {
    val resolved = config.resolve()
    val orig = Seq(
      "waves",
      "metrics"
    ).foldLeft(ConfigFactory.empty()) { case (r, path) => r.withFallback(resolved.withOnlyPath(path)) }

    Seq(
      "waves.custom.genesis",
      "waves.wallet",
      "waves.miner.private-keys",
      "waves.rest-api.api-key-hash",
      "metrics.influx-db"
    ).foldLeft(orig)(_.withoutPath(_))
  }

  def report(config: Config): Unit = {
    val renderOptions = ConfigRenderOptions
      .defaults()
      .setOriginComments(false)
      .setComments(false)
      .setFormatted(false)

    val logInfo: Seq[(String, Any)] = Seq(
      "Available processors" -> Runtime.getRuntime.availableProcessors,
      "Max memory available" -> Runtime.getRuntime.maxMemory
    ) ++ Seq(
      "os.name",
      "os.version",
      "os.arch",
      "java.version",
      "java.vendor",
      "java.home",
      "java.class.path",
      "user.dir",
      "sun.net.inetaddr.ttl",
      "sun.net.inetaddr.negative.ttl",
      "networkaddress.cache.ttl",
      "networkaddress.cache.negative.ttl"
    ).map { x =>
      x -> System.getProperty(x)
    } ++ Seq(
      "Configuration" -> configForLogs(config).root.render(renderOptions)
    )

    log.debug(logInfo.map { case (n, v) => s"$n: $v" }.mkString("\n"))
  }
}
