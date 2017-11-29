package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream}
import java.util.concurrent.locks.ReentrantReadWriteLock

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.db.openDB
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import org.slf4j.bridge.SLF4JBridgeHandler
import scorex.account.AddressScheme
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val configFilename = Try(args(0)).toOption.getOrElse("waves-testnet.conf")
    val outputFilenamePrefix = Try(args(1)).toOption.getOrElse("blockchain")

    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(configFilename))))
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    Try(openDB(settings.dataDirectory)) match {
      case Success(db) =>
        val tryHistory = HistoryWriterImpl(db, new ReentrantReadWriteLock(true), settings.blockchainSettings.functionalitySettings, settings.featuresSettings)
        tryHistory match {
          case Success(history) =>
            val height = history.height()
            log.info(s"Blockchain height: $height")
            val outputFilename = s"$outputFilenamePrefix-$height.waves"
            log.info(s"Output file: $outputFilename")

            createOutputStream(outputFilename) match {
              case Success(output) =>
                val bos = new BufferedOutputStream(output)
                (2 to height).foreach { h =>
                  val maybeBlockBytes = history.blockBytes(h)
                  maybeBlockBytes.foreach { bytes =>
                    val len = bytes.length
                    bos.write(Ints.toByteArray(len))
                    bos.write(bytes)
                    log.info(s"Exported block at height $h: ${len + Ints.BYTES} bytes written")
                  }
                }
                log.info("Done")
                bos.close()
                output.close()

              case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
            }

          case Failure(ex) => log.error(s"Failed to open history at '${settings.dataDirectory}': $ex")
        }

      case Failure(ex) => log.error(s"Failed to open DB at '${settings.dataDirectory}': $ex")
    }
  }

  def createOutputStream(filename: String): Try[FileOutputStream] =
    Try {
      new FileOutputStream(filename)
    }

}

/*

val bos = new BufferedOutputStream(new FileOutputStream(filename))
bos.write(byteArray)
bos.close()


object ImportTool extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val settings = WavesSettings.fromConfig(loadConfig(ConfigFactory.parseFile(new File(args(0)))))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val config = ConfigFactory.parseString("url = \"jdbc:postgresql:waves\"")
    val state = new PostgresWriter(createDataSource(config))

    val history = HistoryWriterImpl(settings.blockchainSettings.blockchainFile, new ReentrantReadWriteLock(true),
      settings.blockchainSettings.functionalitySettings, settings.featuresSettings).get
    val historyHeight = history.height
    val persistedHeight = state.height
    println(s"config file: ${new File(args(0)).toPath.toAbsolutePath}")
    println(s"Blockchain height: $historyHeight, file: ${settings.blockchainSettings.blockchainFile}, persistedHeight: $persistedHeight")

    (persistedHeight + 1 to historyHeight).foldLeft(Option.empty[Block]) {
      case (prevBlock, height) =>
        if (height % 200 == 0) {
          log.debug(s"Imported $height blocks")
        }
        val blockBytes = history.blockBytes(height).get
        val block = Block.parseBytes(blockBytes).get
        require(Signed.validateSignatures(block).isRight, "invalid block signature")
        BlockDiffer.fromBlock(settings.blockchainSettings.functionalitySettings, history, state, prevBlock, block) match {
          case Right(diff) =>
            state.append(diff, block)
            Some(block)
          case Left(e) =>
            println(s"at height $height:")
            throw new Exception(String.valueOf(e))
        }
    }
  }
}
*/
