package scorex

import java.util.logging.Logger

import org.slf4j.LoggerFactory
import scorex.account.Account
import scorex.api.http.ApiClient
import scorex.block.GenesisBlockParams
import Controller
import scorex.settings.Settings
import scorex.transaction.TransactionCreator

import scala.io.StdIn
import scala.util.{Failure, Random, Try}


object Start {

  import Controller.wallet

  def logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]) {
    logger.debug("main " + args)
    if (!args.contains("-cli")) {
      Try {
        if (args.length > 0) Settings.filename = args(0)
        logger.debug("Controller init")
        Controller.init() //STARTING NETWORK/BLOCKCHAIN/RPC
        Thread.sleep(1000)
        testingScript()
      } match {
        case Failure(e) =>
          e.printStackTrace()
          println("STARTUP ERROR: " + e.getMessage)
          System.exit(0) // force all threads shutdown
        case _ =>
          System.exit(0) // force all threads shutdown
      }
    } else {
      println("Welcome to the Score command-line client...")
      Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
        println(s"[$command RESULT] " + ApiClient.executeCommand(command))
      }
    }
  }

  def testingScript(): Unit = {
    Logger.getGlobal.info("Going to execute testing scenario")

    wallet.generateNewAccounts(10)
    wallet.privateKeyAccounts().takeRight(5).foreach(wallet.deleteAccount)

    Logger.getGlobal.info("Executing testing scenario with accounts" +
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

      val tx = TransactionCreator.createPayment(senderAcc, recipientAcc, amt, fee)
      println(s"Payment created: $tx")
    }
  }
}
