package com.wavesplatform.generator.config

import scala.concurrent.duration.{Duration, FiniteDuration}

import com.google.common.base.CaseFormat
import com.typesafe.config.{Config, ConfigRenderOptions}
import com.wavesplatform.generator.Worker
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{TransactionParser, TransactionParsers, TransactionType}
import com.wavesplatform.transaction.TransactionType.TransactionType
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{CollectionReaders, ValueReader}
import play.api.libs.json._

trait FicusImplicits {

  private[this] val byName: Map[String, TransactionParser] = TransactionParsers.all.map { case (_, builder) =>
    builder.getClass.getSimpleName.replaceAll("\\$$", "") -> builder
  }

  private def by(name: String): Option[TransactionParser] = byName.get(name)

  implicit val distributionsReader: ValueReader[Map[TransactionParser, Double]] = {
    val converter                                = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionParser = by(converter.convert(key)).get

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) =>
        toTxType(k) -> v
      }
    }
  }

  implicit val newDistributionsReader: ValueReader[Map[TransactionType, Double]] = {
    val converter                              = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionType = TransactionType.withName(converter.convert(key).replace("Transaction", ""))

    CollectionReaders.mapValueReader[Double].map { xs =>
      xs.map { case (k, v) =>
        toTxType(k) -> v
      }
    }
  }

  implicit val dataEntryReader: ValueReader[DataEntry[_]] = (config: Config, path: String) =>
    Json.parse(config.getConfig(path).root().render(ConfigRenderOptions.concise())).as[DataEntry[_]]

  implicit val workerSettingsReader: ValueReader[Worker.Settings] = (config: Config, path: String) => {
    def readWaitUtxOrDelay(path: String, default: FiniteDuration): Either[FiniteDuration, FiniteDuration] =
      if (config.hasPath(path)) {
        val value = config.as[String](path)
        if (value == "empty-utx") Right(default)
        else {
          val duration: Duration = Duration(value)
          Left(FiniteDuration(duration.length, duration.unit))
        }
      } else Right(default)

    val utxLimit         = config.as[Int](s"$path.utx-limit")
    val delay            = config.as[FiniteDuration](s"$path.delay")
    val tailInitialDelay = readWaitUtxOrDelay(s"$path.tail-initial-delay", delay)
    val initialDelay     = readWaitUtxOrDelay(s"$path.initial-delay", delay)
    val workingTime      = config.as[FiniteDuration](s"$path.working-time")
    val autoReconnect    = config.as[Boolean](s"$path.auto-reconnect")
    val reconnectDelay   = config.as[FiniteDuration](s"$path.reconnect-delay")

    def readWarmUp(warmUpConfig: Config): Worker.WarmUp = {
      val warmUpStart    = warmUpConfig.as[Int](s"start")
      val warmUpEnd      = warmUpConfig.as[Option[Int]](s"end").getOrElse(utxLimit)
      val warmUpStep     = warmUpConfig.as[Int](s"step")
      val warmUpDuration = warmUpConfig.as[Option[FiniteDuration]](s"duration")
      val warmUpOnce     = warmUpConfig.as[Option[Boolean]](s"once").getOrElse(true)
      Worker.WarmUp(warmUpStart, warmUpEnd, warmUpStep, warmUpDuration, warmUpOnce)
    }

    val warmUp     = readWarmUp(config.getConfig(s"$path.warm-up"))
    val initWarmUp = if (config.hasPath(s"$path.initial-warm-up")) Some(readWarmUp(config.getConfig(s"$path.init-warm-up"))) else None

    Worker.Settings(utxLimit, delay, tailInitialDelay, initialDelay, workingTime, autoReconnect, reconnectDelay, warmUp, initWarmUp)
  }
}
