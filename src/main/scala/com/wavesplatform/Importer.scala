package com.wavesplatform

import java.io._
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.db._
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state2.{BlockchainUpdaterImpl, StateStorage, StateWriterImpl}
import com.wavesplatform.utils._
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.block.Block
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  def main(args: Array[String]): Unit = {

    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")
    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    Try(args(1)) match {
      case Success(filename) =>
        log.info(s"Loading file '$filename")
        createInputStream(filename) match {
          case Success(inputStream) =>
            val lock = new ReentrantReadWriteLock()
            val db = openDB(settings.dataDirectory, recreate = true)
            val storage = StateStorage(db, dropExisting = false).get
            val state = new StateWriterImpl(storage, lock)
            val history = HistoryWriterImpl(db, lock, settings.blockchainSettings.functionalitySettings, settings.featuresSettings).get
            val blockchainUpdater = BlockchainUpdaterImpl(state, history, settings, lock)
            checkGenesis(history, settings, blockchainUpdater)
            val bis = new BufferedInputStream(inputStream)
            var quit = false
            val lenBytes = new Array[Byte](Ints.BYTES)
            val start = System.currentTimeMillis()
            while (!quit) {
              val red = bis.read(lenBytes)
              if (red == Ints.BYTES) {
                val len = Ints.fromByteArray(lenBytes)
                val buffer = new Array[Byte](len)
                val s2 = bis.read(buffer)
                if (s2 == len) {
                  val block = Block.parseBytes(buffer).get
                  blockchainUpdater.processBlock(block)
                } else quit = true
              } else quit = true
            }
            bis.close()
            inputStream.close()
            val duration = System.currentTimeMillis() - start
            log.info(s"Imported in ${humanReadableDuration(duration)}")
            db.close()
          case Failure(ex) => log.error(s"Failed to open file '$filename")
        }
      case Failure(ex) => log.error(s"Failed to get input filename from second parameter: $ex")
    }
  }

  def createInputStream(filename: String): Try[FileInputStream] =
    Try {
      new FileInputStream(filename)
    }

}
