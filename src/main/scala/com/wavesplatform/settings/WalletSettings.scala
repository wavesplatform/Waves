package com.wavesplatform.settings

import java.io.File

import com.typesafe.config.Config
import com.wavesplatform.state.ByteStr
import net.ceedubs.ficus.Ficus._

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])

object WalletSettings {
  private val configPath: String = "waves.wallet"

  def fromConfig(config: Config): WalletSettings = {
    val password: String =
      config
        .getAs[String](s"$configPath.password")
        .getOrElse(readPasswordFromConsole())

    val walletFile =
      config
        .as[Option[File]](s"$configPath.file")
    val seed =
      config
        .as[Option[ByteStr]](s"$configPath.seed")

    WalletSettings(walletFile, password, seed)
  }
  def readPasswordFromConsole(): String = {
    val message =
      s"""
        |Cannot obtain password from configuration file.
        |
        |**********************************
        |* Enter password for your wallet *
        |**********************************
      """.stripMargin

    try {
      message.lines
        .foreach(println)

      System
        .console()
        .readPassword("Password > ")
        .mkString
    } catch {
      case _: NullPointerException =>
        throw new Exception(
          """
            |***********************************************************************************************************
            |* Cannot get the console to ask wallet password.                                                          *
            |* Probably, it happens because you trying to start Waves node using supervisor service (like systemd)     *
            |* without specified wallet password.                                                                      *
            |***********************************************************************************************************
          """.stripMargin)
      case ex: Throwable => throw ex
    }
  }
}
