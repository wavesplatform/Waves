package com.wavesplatform

import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.typesafe.config.ConfigFactory
import com.wavesplatform.db.openDB
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state2.{StateStorage, StateWriterImpl}
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

object Dropper extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    Try(openDB(settings.dataDirectory, settings.levelDbCacheSize)) match {
      case Success(db) =>
        log.info("Removing state entries")
        val storage = StateStorage(db, dropExisting = false).get
        val lock = new ReentrantReadWriteLock()
        val state = new StateWriterImpl(storage, lock)
        state.clear()
        db.close()
        log.info("Done")
      case Failure(ex) => log.error(s"Failed to open DB at '${settings.dataDirectory}': $ex")
    }
  }
}
