package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.state.ByteStr
import net.ceedubs.ficus.Ficus._

import scala.io.Source
import scala.util.{Failure, Success, Try}

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])

object WalletSettings {
  private val configPath: String = "waves.wallet"

  def fromConfig(config: Config): WalletSettings = {
    val password: String =
      readPasswordFromConfig(config)
        .fold(
          WalletSettings.readPasswordFromCommandLine,
          identity
        )

    val walletFile = config.as[Option[File]](s"$configPath.file")
    val seed       = config.as[Option[ByteStr]](s"$configPath.seed")

    WalletSettings(walletFile, password, seed)
  }
  def readPasswordFromCommandLine(maybeEx: Throwable): String = {
    val message =
      s"""
        |Cannot obtain password from configuration file: ${maybeEx.getMessage}
        |
        |**********************************
        |* Enter password for your wallet *
        |**********************************
      """.stripMargin

    message.lines
      .foreach(println)

    System
      .console()
      .readPassword("Password > ")
      .mkString
  }

  def readPasswordFromConfig(config: Config): Try[String] = {
    val passwordFromFile = config
      .getAs[File](s"$configPath.password-file")
      .map { file =>
        val buf = Source.fromFile(file)

        Try {
          val password = buf.getLines().next()
          buf.close()
          password
        }
      }

    val passwordFromConfig = config
      .getAs[String](s"$configPath.password")
      .map(Success(_))

    passwordFromFile
      .orElse(passwordFromConfig)
      .getOrElse(Failure(new Exception("No password configuration specified")))
  }
}
