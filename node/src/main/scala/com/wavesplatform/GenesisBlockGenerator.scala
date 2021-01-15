package com.wavesplatform

import java.io.{File, FileNotFoundException}
import java.nio.file.Files
import java.time.Instant

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSCalculator.{generationSignature, hit}
import com.wavesplatform.consensus.{FairPoSCalculator, NxtPoSCalculator, PoSCalculator}
import com.wavesplatform.crypto._
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.{FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.utils._
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.annotation.tailrec
import scala.concurrent.duration._

object GenesisBlockGenerator extends App {

  private type SeedText = String
  private type Share    = Long

  case class DistributionItem(seedText: String, nonce: Int = 0, amount: Share, miner: Boolean = true)

  case class Settings(
      networkType: String,
      baseTarget: Option[Long],
      averageBlockDelay: FiniteDuration,
      timestamp: Option[Long],
      distributions: List[DistributionItem],
      preActivatedFeatures: List[Int] = BlockchainFeatures.implemented.map(_.toInt).toList,
      minBlockTime: Option[FiniteDuration],
      delayDelta: Option[Int]
  ) {

    val initialBalance: Share = distributions.map(_.amount).sum

    val chainId: Char = networkType.head

    val features: Map[Short, Int] =
      preActivatedFeatures.map(_.toShort -> 0).toMap

    val functionalitySettings: FunctionalitySettings = FunctionalitySettings(
      Int.MaxValue,
      Int.MaxValue,
      preActivatedFeatures = features,
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
      minBlockTime = minBlockTime.getOrElse(15.seconds),
      delayDelta = delayDelta.getOrElse(8)
    )

    def preActivated(feature: BlockchainFeature): Boolean = features.contains(feature.id)
  }

  case class FullAddressInfo(
      seedText: SeedText,
      seed: ByteStr,
      accountSeed: ByteStr,
      accountPrivateKey: ByteStr,
      accountPublicKey: ByteStr,
      accountAddress: Address,
      account: KeyPair,
      miner: Boolean
  )

  private def toFullAddressInfo(item: DistributionItem): FullAddressInfo = {
    val seedHash = item.seedText.utf8Bytes
    val acc      = Wallet.generateNewAccount(seedHash, item.nonce)

    FullAddressInfo(
      seedText = item.seedText,
      seed = ByteStr(seedHash),
      accountSeed = ByteStr(acc.seed),
      accountPrivateKey = acc.privateKey,
      accountPublicKey = acc.publicKey,
      accountAddress = acc.toAddress,
      acc,
      item.miner
    )
  }

  val inputConfFile = new File(args.headOption.getOrElse(throw new IllegalArgumentException("Specify a path to genesis.conf")))
  if (!inputConfFile.exists()) throw new FileNotFoundException(inputConfFile.getCanonicalPath)

  val outputConfFile = args
    .drop(1)
    .headOption
    .map(new File(_).getAbsoluteFile.ensuring(f => !f.isDirectory && f.getParentFile.isDirectory || f.getParentFile.mkdirs()))

  val settings: Settings = {
    import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
    ConfigFactory.parseFile(inputConfFile).as[Settings]("genesis-generator")
  }

  com.wavesplatform.account.AddressScheme.current = new AddressScheme {
    override val chainId: Byte = settings.chainId.toByte
  }

  val shares: Seq[(FullAddressInfo, Share)] = settings.distributions
    .map(x => (toFullAddressInfo(x), x.amount))
    .sortBy(_._2)

  val timestamp = settings.timestamp.getOrElse(System.currentTimeMillis())

  val genesisTxs: Seq[GenesisTransaction] = shares.map {
    case (addrInfo, part) =>
      GenesisTransaction(addrInfo.accountAddress, part, timestamp, ByteStr.empty, settings.chainId.toByte)
  }

  report(
    addrInfos = shares.map(_._1),
    settings = genesisSettings(settings.baseTarget),
    settings.chainId,
    settings.features
  )

  private def report(addrInfos: Iterable[FullAddressInfo], settings: GenesisSettings, chainId: Char, preActivatedFeatures: Map[Short, Int]): Unit = {
    val output = new StringBuilder(8192)
    output.append("Addresses:\n")
    addrInfos.foreach { acc =>
      output.append(s"""
             | Seed text:           ${acc.seedText}
             | Seed:                ${acc.seed}
             | Account seed:        ${acc.accountSeed}
             | Private account key: ${acc.accountPrivateKey}
             | Public account key:  ${acc.accountPublicKey}
             | Account address:     ${acc.accountAddress}
             | ===
             |""".stripMargin)
    }

    val walletSettings = addrInfos
      .collect {
        case fai if fai.miner =>
          s"""#  wallet {
         |#    seed = ${fai.seed}
         |#    password =
         |#  }""".stripMargin
      }
      .mkString("\n", "\n", "")

    val confBody = s"""waves {
         |  blockchain.custom {
         |    address-scheme-character = $chainId
         |    functionality {
         |      pre-activated-features = null # undefines all previously defined pre-activated features
         |      pre-activated-features = ${preActivatedFeatures.toSeq.sorted.map { case (f, h) => s"$f = $h" }.mkString("{", ", ", "}")}
         |    }
         |    genesis {
         |      average-block-delay = ${settings.averageBlockDelay.toSeconds}s
         |      initial-base-target = ${settings.initialBaseTarget}
         |      timestamp = ${settings.timestamp} # ${Instant.ofEpochMilli(settings.timestamp)}
         |      block-timestamp = ${settings.blockTimestamp} # ${Instant.ofEpochMilli(settings.blockTimestamp)}
         |      signature = "${settings.signature.get}"
         |      initial-balance = ${settings.initialBalance}
         |      transactions = [
         |        ${settings.transactions.map(x => s"""{recipient = "${x.recipient}", amount = ${x.amount}}""").mkString(",\n    ")}
         |      ]
         |    }
         |  }$walletSettings
         |}
         |""".stripMargin

    output.append("Settings:\n")
    output.append(confBody)
    System.out.print(output.result())
    outputConfFile.foreach(ocf => Files.write(ocf.toPath, confBody.utf8Bytes))
  }

  def genesisSettings(predefined: Option[Long]): GenesisSettings =
    predefined
      .map(baseTarget => mkGenesisSettings(baseTarget))
      .getOrElse(mkGenesisSettings(calcInitialBaseTarget()))

  def mkGenesisSettings(baseTarget: Long): GenesisSettings = {
    val reference     = ByteStr(Array.fill(SignatureLength)(-1: Byte))
    val genesisSigner = KeyPair(ByteStr.empty)

    val genesis = Block
      .buildAndSign(
        version = 1,
        timestamp = timestamp,
        reference = reference,
        baseTarget,
        ByteStr(Array.fill(crypto.DigestLength)(0: Byte)),
        txs = genesisTxs,
        signer = genesisSigner,
        featureVotes = Seq.empty,
        rewardVote = -1L
      )
      .explicitGet()

    GenesisSettings(
      genesis.header.timestamp,
      timestamp,
      settings.initialBalance,
      Some(genesis.signature),
      genesisTxs.map { tx =>
        GenesisTransactionSettings(tx.recipient.stringRepr, tx.amount)
      },
      genesis.header.baseTarget,
      settings.averageBlockDelay
    )
  }

  def calcInitialBaseTarget(): Long = {
    val posCalculator: PoSCalculator =
      if (settings.preActivated(BlockchainFeatures.FairPoS))
        if (settings.preActivated(BlockchainFeatures.BlockV5)) FairPoSCalculator.fromSettings(settings.functionalitySettings)
        else FairPoSCalculator.V1
      else NxtPoSCalculator

    val hitSource = ByteStr(new Array[Byte](crypto.DigestLength))

    def getHit(account: KeyPair): BigInt = {
      val gs = if (settings.preActivated(BlockchainFeatures.BlockV5)) {
        val vrfProof = crypto.signVRF(account.privateKey, hitSource.arr)
        crypto.verifyVRF(vrfProof, hitSource.arr, account.publicKey).map(_.arr).explicitGet()
      } else generationSignature(hitSource, account.publicKey)

      hit(gs)
    }

    shares.collect {
      case (accountInfo, amount) if accountInfo.miner =>
        val hit = getHit(accountInfo.account)

        @tailrec def calculateBaseTarget(keyPair: KeyPair, minBT: Long, maxBT: Long, balance: Long): Long =
          if (maxBT - minBT <= 1) maxBT
          else {
            val newBT = (maxBT + minBT) / 2
            val delay = posCalculator.calculateDelay(hit, newBT, balance)
            if (math.abs(delay - settings.averageBlockDelay.toMillis) < 100) newBT
            else {
              val (min, max) = if (delay > settings.averageBlockDelay.toMillis) (newBT, maxBT) else (minBT, newBT)
              calculateBaseTarget(keyPair, min, max, balance)
            }
          }

        calculateBaseTarget(accountInfo.account, PoSCalculator.MinBaseTarget, 1000000, amount)
    }.max
  }
}
