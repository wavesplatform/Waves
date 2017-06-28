package com.wavesplatform.it.util

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.it.network.client.RawBytes
import org.slf4j.LoggerFactory
import scorex.account.{AddressScheme, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.{CreateAliasTransaction, Transaction}
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.utils.LoggerFacade

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Test extends App {
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
  val n = 10000
  val every = 1.minutes
  log.info(s"Generating $n transactions every $every from addresses:")
  accounts.foreach(a => log.info(a.address))
  val sender = new NetworkSender(new InetSocketAddress(InetAddress.getByName("52.30.47.67"), 6863), 'T', "generator", 38262732757L)
  while (true) {
    // todo сделать настройки распределения
    val txs = TransactionGenerator.gen(accounts, n)
    // todo разложить транзакции в блоки так, чтобы они были валидными
    val txsBySequence = txs.foldLeft[Seq[Seq[Transaction]]](Seq(Seq.empty[Transaction])) { case (s, tx) =>
        def addLastTxAndNewBlock() = s.updated(s.size - 1, s.last :+ tx) :+ Seq()
        tx match {
          case _: IssueTransaction => addLastTxAndNewBlock()
          case _: CreateAliasTransaction => addLastTxAndNewBlock()
          case _: LeaseTransaction => addLastTxAndNewBlock()
          case _ => s.updated(s.size - 1, s.last :+ tx)
        }
    }
    Await.result(sender.sendByNetwork(txs.map(tx => RawBytes(25.toByte, tx.bytes)): _*).map(_ => log.info("Transactions was sent")), Duration.Inf)
    Thread.sleep(every.toMillis)
  }
}
