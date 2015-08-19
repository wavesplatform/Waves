package scorex


import scorex.account.Account
import scorex.app.Controller
import scorex.app.api.http.ApiClient
import scorex.block.GenesisBlockParams
import scorex.app.settings.Settings
import scorex.transaction.TransactionProducer
import scorex.app.utils.ScorexLogging

import scala.io.StdIn
import scala.util.{Failure, Random, Try}


object Start extends App with ScorexLogging {

  import Controller.wallet

  log.debug("main " + args)
  Try {
    if (args.length > 0) Settings.filename = args(0)
    log.debug("Controller init")
    Controller.init() //STARTING NETWORK/BLOCKCHAIN/RPC
    Thread.sleep(1000)
    testingScript()
  } match {
    case Failure(e) =>
      e.printStackTrace()
      log.error("STARTUP ERROR: ", e)
      System.exit(0) // force all threads shutdown
    case _ =>
      System.exit(0) // force all threads shutdown
  }

  def testingScript(): Unit = {
    log.info("Going to execute testing scenario")

    wallet.generateNewAccounts(10)
    wallet.privateKeyAccounts().takeRight(5).foreach(wallet.deleteAccount)

    log.info("Executing testing scenario with accounts" +
      s"(${wallet.privateKeyAccounts().size}) : "
      + wallet.privateKeyAccounts().mkString(" "))

    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(10000)

    (1 to Int.MaxValue).foreach { _ =>
      Thread.sleep(2000)
      val rndIdx = Random.nextInt(GenesisBlockParams.ipoMembers.size)
      val recipientAddress = GenesisBlockParams.ipoMembers(rndIdx)

      val pkAccs = wallet.privateKeyAccounts().ensuring(_.nonEmpty)
      val senderAcc = pkAccs(Random.nextInt(pkAccs.size))
      val recipientAcc = new Account(recipientAddress)

      val amt = Random.nextInt(100000).toLong
      val fee = Random.nextInt(5).toLong

      val tx = TransactionProducer.createPayment(senderAcc, recipientAcc, amt, fee)
      log.info(s"Payment created: $tx")
    }
  }
}
