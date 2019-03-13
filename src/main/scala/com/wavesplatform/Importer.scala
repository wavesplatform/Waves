package com.wavesplatform

import java.io._

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.utx.UtxPool
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import monix.reactive.Observer
import org.slf4j.bridge.SLF4JBridgeHandler

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  def main(args: Array[String]): Unit = {

    SLF4JBridgeHandler.removeHandlersForRootLogger()
    SLF4JBridgeHandler.install()

    val argi = args.iterator
    val (verifyTransactions, configOpt) = {
      Try(argi.next) match {
        case Success("-n") | Success("-no-verify") => (false, Try(argi.next))
        case conf                                  => (true, conf)
      }
    }
    val configFilename     = configOpt.toOption.getOrElse("waves-testnet.conf")
    val blockchainFilename = Try(argi.next)
    val importHeight       = Try(argi.next).map(_.toInt).getOrElse(Int.MaxValue)

    val format = Try(argi.next)
      .map(_.toUpperCase)
      .collect { case custom @ "BINARY_OLD" => custom }
      .getOrElse("BINARY")
      .intern()

    val config   = loadConfig(ConfigFactory.parseFile(new File(configFilename)))
    val settings = WavesSettings.fromConfig(config)
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    implicit val scheduler: Scheduler = Scheduler.singleThread("appender")
    val utxPoolStub = new UtxPool {
      override def putIfNew(tx: Transaction)                               = ???
      override def removeAll(txs: Traversable[Transaction]): Unit          = {}
      override def spendableBalance(addr: Address, assetId: Asset): Long   = ???
      override def pessimisticPortfolio(addr: Address): Portfolio          = ???
      override def all                                                     = ???
      override def size                                                    = ???
      override def transactionById(transactionId: ByteStr)                 = ???
      override def packUnconfirmed(rest: MultiDimensionalMiningConstraint) = ???
      override def close(): Unit                                           = {}
    }

    val time = new NTP(settings.ntpServer)
    blockchainFilename match {
      case Success(filename) =>
        log.info(s"Loading file '$filename'")

        createInputStream(filename) match {
          case Success(inputStream) =>
            val db                = openDB(settings.dataDirectory)
            val blockchainUpdater = StorageFactory(settings, db, time, Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr))
            val pos               = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)
            val extAppender       = BlockAppender(blockchainUpdater, time, utxPoolStub, pos, settings, scheduler, verifyTransactions) _
            checkGenesis(settings, blockchainUpdater)
            val bis           = new BufferedInputStream(inputStream)
            var quit          = false
            val lenBytes      = new Array[Byte](Ints.BYTES)
            val start         = System.currentTimeMillis()
            var counter       = 0
            var blocksToSkip  = blockchainUpdater.height - 1
            val blocksToApply = importHeight - blockchainUpdater.height + 1

            println(s"Skipping $blocksToSkip blocks(s)")

            while (!quit && counter < blocksToApply) {
              val s1 = bis.read(lenBytes)
              if (s1 == Ints.BYTES) {
                val len    = Ints.fromByteArray(lenBytes)
                val buffer = new Array[Byte](len)
                val s2     = bis.read(buffer)
                if (s2 == len) {
                  if (blocksToSkip > 0) {
                    blocksToSkip -= 1
                  } else {
                    val Right(block) =
                      if (format == "BINARY_OLD") Block.parseBytes(buffer).toEither
                      else PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(buffer), unsafe = true)

                    if (blockchainUpdater.lastBlockId.contains(block.reference)) {
                      Await.result(extAppender.apply(block).runAsync, Duration.Inf) match {
                        case Left(ve) =>
                          log.error(s"Error appending block: $ve")
                          quit = true
                        case _ =>
                          counter = counter + 1
                      }
                    }
                  }
                } else {
                  println(s"$s2 != expected $len")
                  quit = true
                }
              } else {
                println(s"Expecting to read ${Ints.BYTES} but got $s1 (${bis.available()})")
                quit = true
              }
            }
            bis.close()
            inputStream.close()
            val duration = System.currentTimeMillis() - start
            log.info(s"Imported $counter block(s) in ${humanReadableDuration(duration)}")
          case Failure(_) => log.error(s"Failed to open file '$filename")
        }
      case Failure(_) => log.error("Usage: Importer [-n | -no-verify] <config file> <blockchain file> [height]")
    }

    time.close()
  }

  private[this] def createInputStream(filename: String) = Try(new FileInputStream(filename))
}
