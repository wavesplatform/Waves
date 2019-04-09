package com.wavesplatform.it.tools

import java.io.File

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.settings._
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

// dex-it/test:runMain com.wavesplatform.it.tools.WalletGeneratorApp dex/src/it/resources/template.conf dex/src/it/container/wallet.dat 0,1
/**
  * Generates wallet.dat with multiple accounts specified by nonce list
  */
object WalletGeneratorApp extends App {
  if (args.length < 2)
    throw new IllegalArgumentException("Please specify parameters: {path-to-node-config} {full/path/to/wallet.dat} {nonce1,nonce2,...}")

  val configFile = new File(args(0))
  if (!configFile.isFile) throw new IllegalArgumentException(s"File '$configFile' does not exist or not a config file")

  val config = ConfigFactory.parseFile(configFile)

  val networkByte = {
    val x = config.getString("waves.blockchain.custom.address-scheme-character")
    if (x.length != 1) throw new IllegalArgumentException(s"Invalid network byte in '$configFile': $x")
    x.head.toByte
  }

  val walletSettings = config.as[WalletSettings]("waves.wallet")
  val walletFile     = new File(args(1))
  val nonceList      = args(2).split(',').map(_.toInt)

  walletFile.delete()
  println(s"""Wallet file:  ${walletFile.getAbsolutePath}
       |Network byte: '${networkByte.toChar}'
       |Nonce list:   ${nonceList.mkString(", ")}""".stripMargin)

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = networkByte
  }

  val w = Wallet(walletSettings.copy(file = Some(walletFile)))
  nonceList.foreach(w.generateNewAccount)
  println(s"Added addresses: ${w.privateKeyAccounts.map(_.toAddress).mkString(", ")}")
  println("Done")
}
