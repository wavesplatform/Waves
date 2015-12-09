package scorex.lagonaki.server

import scorex.transaction.GenesisTransaction
import scorex.utils.ScorexLogging

import scala.util.{Failure, Random, Try}
import scala.concurrent.duration._


object Server extends App with ScorexLogging {

  log.debug("Start server with args: {} ", args)
  Try {
    val filename = if (args.length > 0) args(0) else "settings.json"

    val application = new LagonakiApplication(filename)

    log.debug("LagonakiApplication has been started")
    application.run()
    if(application.settings.offlineGeneration) {
      testingScript(application)
    } else {
      Thread.sleep(30.seconds.toMillis)
      testingScript(application)
    }
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

    if(wallet.privateKeyAccounts().isEmpty) {
      wallet.generateNewAccounts(10)
      println("pkas:")
      wallet.privateKeyAccounts().toList.map(_.address).foreach(println)
      wallet.privateKeyAccounts().takeRight(5).foreach(wallet.deleteAccount)
    }

    log.info("Executing testing scenario with accounts" +
      s"(${wallet.privateKeyAccounts().size}) : "
      + wallet.privateKeyAccounts().mkString(" "))

    require(wallet.privateKeyAccounts().nonEmpty)

    Thread.sleep(3000)

    val genesisBlock = application.blockStorage.history.genesis
    val genesisAccs = genesisBlock.transactions.flatMap { tx => tx match {
      case gtx: GenesisTransaction =>
        Some(gtx.recipient)
      case _ =>
        log.error("Non-genesis tx in the genesis block!")
        None
    })

    (1 to Int.MaxValue).foreach { _ =>
      Thread.sleep(1000)
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
