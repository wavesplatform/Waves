package scorex


import scorex.app.LagonakiApplication
import scorex.transaction.GenesisTransaction
import scorex.utils.ScorexLogging

import scala.util.{Failure, Random, Try}


object Server extends App with ScorexLogging {

  log.debug("main " + args)
  Try {
    val filename = if (args.length > 0) args(0) else "settings.json"

    val application = new LagonakiApplication(filename)

    log.debug("LagonakiApplication run")
    application.run() //STARTING NETWORK/BLOCKCHAIN/RPC
    Thread.sleep(10000)
    testingScript(application)
  } match {
    case Failure(e) =>
      e.printStackTrace()
      log.error("STARTUP ERROR: ", e)
      System.exit(0)
    case _ =>
      System.exit(0)
  }

  def testingScript(application: LagonakiApplication): Unit = {
    log.info("Going to execute testing scenario")
    val wallet = application.wallet

    wallet.generateNewAccounts(10)
    wallet.privateKeyAccounts().takeRight(5).foreach(wallet.deleteAccount)

    log.info("Executing testing scenario with accounts" +
      s"(${wallet.privateKeyAccounts().size}) : "
      + wallet.privateKeyAccounts().mkString(" "))

    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(10000)

    val genesisBlock = application.blockchainStorage.blockAt(1)
    val genesisAccs = genesisBlock.get.transactions.flatMap { tx => tx match {
      case gtx: GenesisTransaction =>
        Some(gtx.recipient)
      case _ =>
        log.error("Non-genesis tx in the genesis block!")
        None
    }
    }

    (1 to Int.MaxValue).foreach { _ =>
      Thread.sleep(2000)
      val pkAccs = wallet.privateKeyAccounts().ensuring(_.nonEmpty)
      val senderAcc = pkAccs(Random.nextInt(pkAccs.size))
      val recipientAcc = genesisAccs(Random.nextInt(genesisAccs.size))

      val amt = Random.nextInt(100000).toLong
      val fee = Random.nextInt(5).toLong

      val tx = application.createPayment(senderAcc, recipientAcc, amt, fee)
      log.info(s"Payment created: $tx")
    }
  }
}
