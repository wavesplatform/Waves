package scorex

import api.ApiClient
import controller.Controller
import scorex.account.Account
import scorex.block.GenesisBlockParams
import scorex.crypto.Base58
import scorex.database.wallet.SecureWalletDatabase
import scorex.wallet.Wallet

import scala.io.StdIn
import scala.util.{Failure, Random, Try}

object Start {

  def main(args: Array[String]) {
    if (!args.contains("-cli")) {
      Try {
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
    Wallet.create(Base58.decode("FQgbSAm6swGbtqA3NE8PttijPhT4N3Ufh4bHFAkyVnQz"), "cookies", 10)
    //require(Wallet.unlock("cookies"))

    require(SecureWalletDatabase.exists())
    require(Wallet.privateKeyAccounts().nonEmpty)

    (1 to Int.MaxValue).foreach { _ =>
      val rndIdx = Random.nextInt(GenesisBlockParams.ipoMembers.size - 1)
      val senderAddress = GenesisBlockParams.ipoMembers(rndIdx)
      val recipientAddress = GenesisBlockParams.ipoMembers(rndIdx + 1)

      val senderAcc = Wallet.privateKeyAccount(senderAddress).get
      val recipientAcc = new Account(recipientAddress)

      val amt = new java.math.BigDecimal(Random.nextInt(100000))
      val fee = new java.math.BigDecimal(1 + Random.nextInt(5))

      val (tx, valRes) = Controller.sendPayment(senderAcc, recipientAcc, amt, fee)
      println(s"Payment created: $tx, validationResult: $valRes")
      Thread.sleep(120000) // too short delays causes bug, see comments in PaymentTransaction
    }
  }
}