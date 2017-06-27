package com.wavesplatform.it.util

import java.net.{InetAddress, InetSocketAddress}

import com.wavesplatform.it.network.client.RawBytes
import org.slf4j.LoggerFactory
import scorex.account.{AddressScheme, PrivateKeyAccount}
import scorex.crypto.encode.Base58
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
  val sender = new NetworkSender(new InetSocketAddress(InetAddress.getByName("52.51.92.182"), 6863), 'T', "generator", 38268732757L)
  while (true) {
    val txs = TransactionGenerator.gen(accounts, 10000)
    // todo 5
    // todo 6
    // todo 9
    Await.result(sender.sendByNetwork(txs.map(tx => RawBytes(25.toByte, tx.bytes)): _*).map(_ => log.info("Transactions was sent")), Duration.Inf)
    Thread.sleep(every.toMillis)
  }
}
