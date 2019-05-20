package com.wavesplatform
import java.io.BufferedInputStream

import akka.actor.ActorSystem
import com.wavesplatform.Importer.{initBlockchain, initFileStream, initTime, initUtxPool, loadSettings, parseArgs, startImport}
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, BlockchainUpdated}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, Transaction}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.execution.Scheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.util.{Failure, Success, Try}

object ImporterSending extends ScorexLogging {
  def initBlockchainUpdatesExtension(wavesSettings: WavesSettings, blockchainUpdatedObservable: Observable[BlockchainUpdated]): Extension = {
    val extensionClass = Class.forName("com.wavesplatform.events.BlockchainUpdates").asInstanceOf[Class[Extension]]
    val ctor           = extensionClass.getConstructor(classOf[Context])

    val extensionContext = new Context {
      override def settings: WavesSettings                                  = wavesSettings
      override def blockchain: Blockchain with BlockchainUpdater            = ???
      override def time: Time                                               = ???
      override def wallet: Wallet                                           = ???
      override def utx: UtxPool                                             = ???
      override def broadcastTx(tx: Transaction): Unit                       = ???
      override def spendableBalanceChanged: Observable[(Address, Asset)]    = ???
      override def actorSystem: ActorSystem                                 = ???
      override def blockchainUpdated: Option[Observable[BlockchainUpdated]] = Some(blockchainUpdatedObservable)
    }

    ctor.newInstance(extensionContext)
  }

  def run(args: Array[String]): Try[() => Unit] =
    for {
      importerSettings <- parseArgs(args)
      wavesSettings = loadSettings(importerSettings.configFilename)
      _ = AddressScheme.current = new AddressScheme {
        override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
      }

      fis <- initFileStream(importerSettings.blockchainFilename)
      bis = new BufferedInputStream(fis)

      scheduler = Scheduler.singleThread("appender")

      sendingScheduler  = Scheduler.singleThread("blockchain-updates")
      blockchainUpdated = ConcurrentSubject.publish[BlockchainUpdated](sendingScheduler)
      blockchainUpdatesExtension = {
        val e = initBlockchainUpdatesExtension(wavesSettings, blockchainUpdated)
        e.start()
        e
      }

      time                             = initTime(wavesSettings.ntpServer)
      utxPool                          = initUtxPool()
      (blockchainUpdater, appendBlock) = initBlockchain(scheduler, time, utxPool, wavesSettings, importerSettings, Some(blockchainUpdated))
      _                                = startImport(scheduler, bis, blockchainUpdater, appendBlock, importerSettings)
    } yield
      () => {
        blockchainUpdated.onComplete()
        Await.result(blockchainUpdatesExtension.shutdown(), 10.seconds)
        bis.close()
        fis.close()
        time.close()
        utxPool.close()
      }

  def main(args: Array[String]): Unit = {
    run(args) match {
      case Success(shutdown) => shutdown()
      case Failure(ext)      => log.error(ext.getMessage)
    }
  }
}
