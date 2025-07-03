package com.wavesplatform.generator.config

import com.google.common.base.CaseFormat
import com.typesafe.config.*
import com.wavesplatform.generator.Worker
import com.wavesplatform.settings.*
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.TransactionType
import com.wavesplatform.transaction.TransactionType.TransactionType
import play.api.libs.json.*
import pureconfig.*
import pureconfig.error.ThrowableFailure

import scala.concurrent.duration.FiniteDuration
import scala.util.control.NonFatal

trait ConfigReaders {

  given ConfigReader[Map[TransactionType, Double]] = {
    val converter                              = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)
    def toTxType(key: String): TransactionType = TransactionType.withName(converter.convert(key).replace("Transaction", ""))

    CollectionReaders.mapReader[Double].map { xs =>
      xs.map { case (k, v) =>
        toTxType(k) -> v
      }
    }
  }

  given ConfigReader[DataEntry[?]] =
    ConfigReader.fromFunction(v =>
      try Right(Json.parse(v.render(ConfigRenderOptions.concise())).as[DataEntry[?]])
      catch {
        case NonFatal(e) => ConfigReader.Result.fail(ThrowableFailure(e, Some(v.origin())))
      }
    )

  given ConfigReader[Worker.Settings] = ConfigReader.fromCursor { v =>
    def readInitialDelay(obj: ConfigObjectCursor, path: String, delay: FiniteDuration): ConfigReader.Result[Either[FiniteDuration, FiniteDuration]] =
      for {
        delayStr <- obj.optionalWithDefault[String](path, "empty-utx")
        delay    <- if (delayStr == "empty-utx") Right(Right(delay)) else obj.required[FiniteDuration]("initial-delay").map(d => Left(d))
      } yield delay

    def readWarmUp(warmUpConfig: ConfigObjectCursor, utxLimit: Int): ConfigReader.Result[Worker.WarmUp] = for {
      warmUpStart    <- warmUpConfig.required[Int]("start")
      warmUpEnd      <- warmUpConfig.optionalWithDefault[Int]("end", utxLimit)
      warmUpStep     <- warmUpConfig.required[Int]("step")
      warmUpDuration <- warmUpConfig.optionalWithDefault[Option[FiniteDuration]]("duration", None)
      warmUpOnce     <- warmUpConfig.optionalWithDefault[Boolean]("once", true)
    } yield Worker.WarmUp(warmUpStart, warmUpEnd, warmUpStep, warmUpDuration, warmUpOnce)

    for {
      obj            <- v.asObjectCursor
      utxLimit       <- obj.required[Int]("utx-limit")
      delay          <- obj.required[FiniteDuration]("delay")
      workingTime    <- obj.required[FiniteDuration]("working-time")
      autoReconnect  <- obj.required[Boolean]("auto-reconnect")
      reconnectDelay <- obj.required[FiniteDuration]("reconnect-delay")
      warmUpObj      <- obj.atKey("warm-up").flatMap(_.asObjectCursor)
      warmUp         <- readWarmUp(warmUpObj, utxLimit)
      initWarmUp <- obj
        .atKeyOrUndefined("init-warm-up")
        .asObjectCursor
        .fold[ConfigReader.Result[Option[Worker.WarmUp]]](_ => Right(None), v => readWarmUp(v, utxLimit).map(v => Some(v)))
      initialDelay     <- readInitialDelay(obj, "initial-delay", delay)
      tailInitialDelay <- readInitialDelay(obj, "tail-initial-delay", delay)
    } yield Worker.Settings(utxLimit, delay, tailInitialDelay, initialDelay, workingTime, autoReconnect, reconnectDelay, warmUp, initWarmUp)
  }
}
