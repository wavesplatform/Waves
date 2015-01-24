package scorex.main

import api.ApiClient
import controller.Controller
import scorex.block.GenesisBlockParams
import scala.io.StdIn
import scala.util.{Random, Failure, Try}

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
    Controller.unlockWallet("cookies")

    //require(Controller.getPrivateKeyAccounts().nonEmpty)

    Thread.sleep(10000)

    (1 to Int.MaxValue).foreach { _ =>
      val rndIdx = Random.nextInt(GenesisBlockParams.ipoMembers.size - 1)
      val senderAddress = GenesisBlockParams.ipoMembers(rndIdx)
      val recipientAddress = GenesisBlockParams.ipoMembers(rndIdx + 1)

      val senderAcc = Controller.getPrivateKeyAccountByAddress(senderAddress)
      val recipientAcc = Controller.getAccountByAddress(recipientAddress)

      val amt = new java.math.BigDecimal(Random.nextInt(100000))
      val fee = new java.math.BigDecimal(1 + Random.nextInt(5))

      val (tx, valRes) = Controller.sendPayment(senderAcc.get, recipientAcc, amt, fee)
      println(s"Payment created: $tx, validationResult: $valRes")
      Thread.sleep(120000) // too short delays causes bug, see comments in PaymentTransaction
    }
  }
}