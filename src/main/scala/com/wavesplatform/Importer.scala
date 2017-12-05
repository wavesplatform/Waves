package com.wavesplatform

import java.io._

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.settings.{WavesSettings, loadConfig}
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
            deleteFile(settings.blockchainSettings.blockchainFile)
            deleteFile(settings.blockchainSettings.stateFile)
            val (history, _, stateWriter, _, blockchainUpdater, _) = StorageFactory(settings).get
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
            stateWriter.close()
            history.close()
            val duration = System.currentTimeMillis() - start
            log.info(s"Imported in ${humanReadableDuration(duration)}")
          case Failure(ex) => log.error(s"Failed to open file '$filename")
        }
      case Failure(ex) => log.error(s"Failed to get input filename from second parameter: $ex")
    }
  }

  def createInputStream(filename: String): Try[FileInputStream] =
    Try {
      new FileInputStream(filename)
    }

  def deleteFile(maybeFile: Option[File]): Unit = maybeFile.foreach { f =>
    f.delete()
  }
}
