package com.wavesplatform.it.util

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.transactions.TransactionsGeneratorTest
import com.wavesplatform.network.RawBytes
import org.slf4j.LoggerFactory
import scorex.account.{AddressScheme, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser.{TransactionType => TT}
import scorex.utils.LoggerFacade

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TestnetTransactionsGeneratorTest extends App {
  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'T'
  }
  val log = LoggerFacade(LoggerFactory.getLogger(getClass))

  val accounts = Seq(
    PrivateKeyAccount(Base58.decode("f8ypmbNfr6ocg8kJ7F1MaC4A89f672ZY6LETRiAEbrb").get),
    PrivateKeyAccount(Base58.decode("A7ucz2PXvDx3CCDYRhtRkHHzPZZnCTP4T6mN18QvsfCQ").get),
    PrivateKeyAccount(Base58.decode("uD933WUgrNzhFE2amLT6pijejUm2bTocKqU1Nh3D3rk").get),
    PrivateKeyAccount(Base58.decode("63HeWce3hCmeTSxciqG3MsHSp9A1B8jueeS6NmoGAu3F").get)
  )

  val n = 150
  val every = 10.seconds

  // Transactions are sent in a row without waiting for entry into blocks, so Burn, Reissue, LeaseCancel transactions will be invalid
  val txTypesProbabilities = Map(
    TT.IssueTransaction -> 0.1f,
    TT.CreateAliasTransaction -> 0.1f,
    TT.LeaseTransaction -> 0.1f,
    TT.TransferTransaction -> 0.3f,
    TT.ReissueTransaction -> 0.05f,
    TT.BurnTransaction -> 0.075f,
    TT.LeaseCancelTransaction -> 0.075f,
    TT.ExchangeTransaction -> 0.1f,
    TT.PaymentTransaction -> 0.1f
  )

  log.info(s"Generating $n transactions every $every from addresses:")
  accounts.foreach(a => log.info(a.address))
  val sender = new NetworkSender(new InetSocketAddress(InetAddress.getByName("127.0.0.1"), 6863), 'T', "generator", 38262732757L)
  val api = NodeApi.create("127.0.0.1", 6869, 0, 1.minute)
  sys.addShutdownHook(sender.close())
//  while (true) {
    val txs = TransactionGenerator.gen(txTypesProbabilities, accounts, n)
    TransactionsGeneratorTest.splitTransactionsIntoValidBlocks(txs).foreach{txs =>
      txs.foreach(t => println(t.id, t.toString))
      Await.result(sender.sendByNetwork(txs.map(tx => RawBytes(25.toByte, tx.bytes)): _*).map(_ => log.info("Transactions was sent")), Duration.Inf)
      Await.result(api.waitFor[Seq[NodeApi.Transaction]](api.utx, _.isEmpty, 5.seconds), Duration.Inf)
    }
//    Thread.sleep(every.toMillis)
//  }
}
