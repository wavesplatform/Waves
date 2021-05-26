package com.wavesplatform

import java.io.{BufferedOutputStream, File, FileOutputStream, OutputStream}

import com.google.common.primitives.Ints
import com.wavesplatform.database.{DBExt, openDB}
import com.wavesplatform.events.BlockchainUpdateTriggers
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.metrics.Metrics
import com.wavesplatform.state.Height
import com.wavesplatform.utils._
import kamon.Kamon
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.DB
import scopt.OParser

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object Exporter extends ScorexLogging {
  //noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ExporterOptions()).foreach {
      case ExporterOptions(configFile, outputFileNamePrefix, exportHeight) =>
        implicit val reporter: UncaughtExceptionReporter = UncaughtExceptionReporter.default

        val settings = Application.loadApplicationConfig(configFile)

        val time             = new NTP(settings.ntpServer)
        val db               = openDB(settings.dbSettings.directory)
        val (blockchain, _)  = StorageFactory(settings, db, time, Observer.empty, BlockchainUpdateTriggers.noop)
        val blockchainHeight = blockchain.height
        val height           = Math.min(blockchainHeight, exportHeight.getOrElse(blockchainHeight))
        log.info(s"Blockchain height is $blockchainHeight exporting to $height")
        val outputFilename = s"$outputFileNamePrefix-$height"
        log.info(s"Output file: $outputFilename")

        IO.createOutputStream(outputFilename) match {
          case Success(output) =>
            var exportedBytes = 0L
            val bos           = new BufferedOutputStream(output, 10 * 1024 * 1024)
            val start         = System.currentTimeMillis()
            (2 to height).foreach { h =>
              exportedBytes += IO.exportBlockToBinary(bos, db, h, legacy = true)
              if (h % (height / 10) == 0)
                log.info(s"$h blocks exported, ${humanReadableSize(exportedBytes)} written")
            }
            val duration = System.currentTimeMillis() - start
            log.info(s"Finished exporting $height blocks in ${humanReadableDuration(duration)}, ${humanReadableSize(exportedBytes)} written")
            bos.close()
            output.close()
          case Failure(ex) => log.error(s"Failed to create file '$outputFilename': $ex")
        }

        Try(Await.result(Kamon.stopModules(), 10.seconds))
        Metrics.shutdown()
        time.close()
    }
  }

  private[this] object IO {
    def createOutputStream(filename: String): Try[FileOutputStream] =
      Try(new FileOutputStream(filename))

    def exportBlockToBinary(stream: OutputStream, db: DB, height: Int, legacy: Boolean): Int = {
      val maybeBlockBytes = db.readOnly(ro => database.loadBlock(Height(height), ro)).map(_.bytes())
      maybeBlockBytes
        .map { bytes =>
          val bytesLength = bytes.length

          stream.write(Ints.toByteArray(bytesLength))
          stream.write(bytes)

          Ints.BYTES + bytesLength
        }
        .getOrElse(0)
    }
  }

  private[this] final case class ExporterOptions(
      configFileName: Option[File] = None,
      outputFileNamePrefix: String = "blockchain",
      exportHeight: Option[Int] = None
  )

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[ExporterOptions]
    import builder._

    OParser.sequence(
      programName("waves export"),
      head("Waves Blockchain Exporter", Version.VersionString),
      opt[File]('c', "config")
        .text("Node config file path")
        .action((f, c) => c.copy(configFileName = Some(f))),
      opt[String]('o', "output-prefix")
        .text("Output file name prefix")
        .action((p, c) => c.copy(outputFileNamePrefix = p)),
      opt[Int]('h', "height")
        .text("Export to height")
        .action((h, c) => c.copy(exportHeight = Some(h)))
        .validate(h => if (h > 0) success else failure("Export height must be > 0")),
      opt[Int]('h', "height")
        .text("Export to height")
        .action((h, c) => c.copy(exportHeight = Some(h)))
        .validate(h => if (h > 0) success else failure("Export height must be > 0")),
      help("help").hidden()
    )
  }
}
