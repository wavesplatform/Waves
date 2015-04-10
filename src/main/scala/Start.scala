package scorex

import java.util.logging.Logger
import api.ApiClient
import controller.Controller
import scorex.account.Account
import scorex.block.GenesisBlockParams
import scorex.crypto.Base58
import scorex.wallet.Wallet
import settings.Settings

import scala.io.StdIn
import scala.util.{Failure, Random, Try}

object Start {

  def main(args: Array[String]) {
    if (!args.contains("-cli")) {
      Try {
        if (args.length > 0) Settings.filename = args(0)
        Controller.init() //STARTING NETWORK/BLOCKCHAIN/RPC
        testingScript()
      } match {
        case Failure(e) =>
          e.printStackTrace()
          println("STARTUP ERROR: " + e.getMessage)
          System.exit(0) // force all threads shutdown
        case _ =>
      }
    } else {
      println("Welcome to the client...")
      Iterator.continually(StdIn.readLine()).takeWhile(!_.equals("quit")).foreach { command =>
        println(s"[$command RESULT] " + ApiClient.executeCommand(command))
      }
    }
  }


  def testingScript(): Unit = {
    val NumOfAccounts = 10

    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", NumOfAccounts)
    Wallet.privateKeyAccounts().flatMap { acc =>
      if (Random.nextBoolean()) Some(acc) else None
    }.map(Wallet.deleteAccount)
    Logger.getGlobal.info("Executing testing scenario with accounts" +
      s"(${Wallet.privateKeyAccounts().size}) : "
      + Wallet.privateKeyAccounts().mkString(" "))

    //require(Wallet.unlock("cookies"))

    require(Wallet.exists())
    require(Wallet.privateKeyAccounts().nonEmpty)

    (1 to Int.MaxValue).foreach { _ =>
      val rndIdx = Random.nextInt(GenesisBlockParams.ipoMembers.size)
      val recipientAddress = GenesisBlockParams.ipoMembers(rndIdx)

      val pkAccs = Wallet.privateKeyAccounts()
      val senderAcc = pkAccs(Random.nextInt(pkAccs.size))
      val recipientAcc = new Account(recipientAddress)

      val amt = new java.math.BigDecimal(Random.nextInt(100000))
      val fee = new java.math.BigDecimal(1 + Random.nextInt(5))

      val (tx, valRes) = Controller.sendPayment(senderAcc, recipientAcc, amt, fee)
      println(s"Payment created: $tx, validationResult: $valRes")
      Thread.sleep(20000) // too short delays causes bug, see comments in PaymentTransaction
    }
  }
}