package com.wavesplatform

import java.io.{File, FileNotFoundException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.crypto._
import com.wavesplatform.settings.{GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.concurrent.duration._

object GenesisBlockGenerator extends App {

  private type SeedText = String
  private type Share    = Long

  case class Settings(networkType: String,
                      initialBalance: Long,
                      baseTarget: Long,
                      averageBlockDelay: FiniteDuration,
                      timestamp: Option[Long],
                      distributions: Map[SeedText, Share]) {

    private[this] val distributionsSum = distributions.values.sum
    require(
      distributionsSum == initialBalance,
      s"The sum of all balances should be == $initialBalance, but it is $distributionsSum"
    )

    val chainId: Byte = networkType.head.toByte
  }

  case class FullAddressInfo(seedText: SeedText,
                             seed: ByteStr,
                             accountSeed: ByteStr,
                             accountPrivateKey: ByteStr,
                             accountPublicKey: ByteStr,
                             accountAddress: Address)

  private def toFullAddressInfo(seedText: SeedText): FullAddressInfo = {
    val seedHash = seedText.getBytes
    val acc      = Wallet.generateNewAccount(seedHash, 0)

    FullAddressInfo(
      seedText = seedText,
      seed = ByteStr(seedHash),
      accountSeed = ByteStr(acc.seed),
      accountPrivateKey = acc.privateKey,
      accountPublicKey = acc.publicKey,
      accountAddress = acc.toAddress
    )
  }

  val inputConfFile = new File(args.headOption.getOrElse(throw new IllegalArgumentException("Specify a path to genesis.conf")))
  if (!inputConfFile.exists()) throw new FileNotFoundException(inputConfFile.getCanonicalPath)

  val outputConfFile = args
    .drop(1)
    .headOption
    .map(new File(_).ensuring(f => f.getParentFile.isDirectory || f.getParentFile.mkdirs()))

  val settings: Settings = {
    implicit val _ = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
    ConfigFactory.parseFile(inputConfFile).as[Settings]("genesis-generator")
  }

  com.wavesplatform.account.AddressScheme.current = new AddressScheme {
    override val chainId: Byte = settings.chainId
  }

  val shares: Map[FullAddressInfo, Share] = {
    settings.distributions.map { case (seedText, part) => toFullAddressInfo(seedText) -> part }
  }

  val timestamp = settings.timestamp.getOrElse(System.currentTimeMillis())

  val genesisTxs: Seq[GenesisTransaction] = shares.map {
    case (addrInfo, part) =>
      GenesisTransaction(addrInfo.accountAddress, part, timestamp, ByteStr.empty)
  }.toSeq

  val genesisBlock: Block = {
    val reference     = ByteStr(Array.fill(SignatureLength)(-1: Byte))
    val genesisSigner = KeyPair(ByteStr.empty)

    Block
      .buildAndSign(
        version = 1,
        timestamp = timestamp,
        reference = reference,
        consensusData = NxtLikeConsensusBlockData(settings.baseTarget, ByteStr(Array.fill(crypto.DigestSize)(0: Byte))),
        transactionData = genesisTxs,
        signer = genesisSigner,
        featureVotes = Set.empty
      )
      .explicitGet()
  }

  val signature = genesisBlock.signerData.signature

  report(
    addrInfos = shares.keysIterator,
    settings = GenesisSettings(
      timestamp,
      genesisBlock.timestamp,
      settings.initialBalance,
      Some(signature),
      genesisTxs.map { tx =>
        GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)
      },
      settings.baseTarget,
      settings.averageBlockDelay
    )
  )

  private def report(addrInfos: Iterator[FullAddressInfo], settings: GenesisSettings): Unit = {
    val output = new StringBuilder(8192)
    output.append("Addresses:\n")
    addrInfos.zipWithIndex.foreach {
      case (acc, n) =>
        output.append(s"""($n):
           | Seed text:           ${acc.seedText}
           | Seed:                ${acc.seed}
           | Account seed:        ${acc.accountSeed}
           | Private account key: ${acc.accountPrivateKey}
           | Public account key:  ${acc.accountPublicKey}
           | Account address:     ${acc.accountAddress}
           |
           |""".stripMargin)
    }

    val confBody = s"""genesis {
                      |  average-block-delay: ${settings.averageBlockDelay.toMillis}ms
                      |  initial-base-target: ${settings.initialBaseTarget}
                      |  timestamp: ${settings.timestamp}
                      |  block-timestamp: ${settings.blockTimestamp}
                      |  signature: "${settings.signature.get}"
                      |  initial-balance: ${settings.initialBalance}
                      |  transactions = [
                      |    ${settings.transactions
      .map { x =>
        s"""{recipient: "${x.recipient}", amount: ${x.amount}}"""
      }
      .mkString(",\n    ")}
                      |  ]
                      |}
                      |""".stripMargin

    output.append("Settings:\n")
    output.append(confBody)
    System.out.print(output.result())
    outputConfFile.foreach(ocf => Files.write(ocf.toPath, confBody.getBytes(StandardCharsets.UTF_8)))
  }
}
