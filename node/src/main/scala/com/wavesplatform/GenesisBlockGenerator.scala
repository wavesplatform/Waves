package com.wavesplatform

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.{Address, AddressScheme, KeyPair, SeedKeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSCalculator.{generationSignature, hit}
import com.wavesplatform.consensus.{FairPoSCalculator, NxtPoSCalculator}
import com.wavesplatform.crypto.*
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.settings.{FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.transaction.{GenesisTransaction, TxNonNegativeAmount}
import com.wavesplatform.utils.*
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*

import java.io.{File, FileNotFoundException}
import java.nio.file.Files
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

object GenesisBlockGenerator {

  private type SeedText = String
  private type Share    = Long

  case class DistributionItem(seedText: String, nonce: Int, amount: Share, miner: Boolean = true)

  case class Settings(
      networkType: String,
      baseTarget: Option[Long],
      averageBlockDelay: FiniteDuration,
      timestamp: Option[Long],
      distributions: List[DistributionItem],
      preActivatedFeatures: Option[List[Int]],
      minBlockTime: Option[FiniteDuration],
      delayDelta: Option[Int]
  ) {

    val initialBalance: Share = distributions.map(_.amount).sum

    val chainId: Byte = networkType.head.toByte

    private val features: Map[Short, Int] =
      preActivatedFeatures.getOrElse(List(BlockchainFeatures.FairPoS.id.toInt, BlockchainFeatures.BlockV5.id.toInt)).map(f => f.toShort -> 0).toMap

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
      account: SeedKeyPair,
      miner: Boolean
  )

  def toFullAddressInfo(item: DistributionItem): FullAddressInfo = {
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

  def main(args: Array[String]): Unit = {
    val inputConfFile = new File(args.headOption.getOrElse(throw new IllegalArgumentException("Specify a path to genesis.conf")))
    if (!inputConfFile.exists()) throw new FileNotFoundException(inputConfFile.getCanonicalPath)

    val outputConfFile = args
      .drop(1)
      .headOption
      .map(new File(_).getAbsoluteFile.ensuring(f => !f.isDirectory && f.getParentFile.isDirectory || f.getParentFile.mkdirs()))

    val settings = parseSettings(ConfigFactory.parseFile(inputConfFile).resolve())
    val confBody = createConfig(settings)
    outputConfFile.foreach(ocf => Files.write(ocf.toPath, confBody.utf8Bytes))
  }

  def parseSettings(config: Config): Settings = {
    import net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
    config.as[Settings]("genesis-generator")
  }

  def createConfig(settings: Settings): String = {
    def generateAndReport(addrInfos: Iterable[FullAddressInfo], settings: GenesisSettings): String = {
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

      val confBody = s"""genesis {
                        |  average-block-delay = ${settings.averageBlockDelay.toMillis}ms
                        |  initial-base-target = ${settings.initialBaseTarget}
                        |  timestamp = ${settings.timestamp}
                        |  block-timestamp = ${settings.blockTimestamp}
                        |  signature = "${settings.signature.get}"
                        |  initial-balance = ${settings.initialBalance}
                        |  transactions = [
                        |    ${settings.transactions.map(x => s"""{recipient = "${x.recipient}", amount = ${x.amount}}""").mkString(",\n    ")}
                        |  ]
                        |}
                        |""".stripMargin

      output.append("Settings:\n")
      output.append(confBody)
      System.out.print(output.result())
      confBody
    }

    com.wavesplatform.account.AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.chainId
    }

    val shares: Seq[(FullAddressInfo, Share)] = settings.distributions
      .map(x => (toFullAddressInfo(x), x.amount))
      .sortBy(_._2)
    val minerShares = shares.filter(_._1.miner)

    val timestamp = settings.timestamp.getOrElse(System.currentTimeMillis())

    val genesisTxs: Seq[GenesisTransaction] = shares.flatMap { case (addrInfo, part) =>
      TxNonNegativeAmount
        .from(part)
        .toOption
        .map(amount => GenesisTransaction(addrInfo.accountAddress, amount, timestamp, ByteStr.empty, settings.chainId))
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
          rewardVote = -1L,
          stateHash = None,
          challengedHeader = None
        )
        .explicitGet()

      GenesisSettings(
        genesis.header.timestamp,
        timestamp,
        settings.initialBalance,
        Some(genesis.signature),
        genesisTxs.map { tx =>
          GenesisTransactionSettings(tx.recipient.toString, tx.amount.value)
        },
        genesis.header.baseTarget,
        settings.averageBlockDelay
      )
    }

    def calcInitialBaseTarget(): Long = {
      val posCalculator =
        if (settings.preActivated(BlockchainFeatures.FairPoS))
          if (settings.preActivated(BlockchainFeatures.BlockV5)) FairPoSCalculator.fromSettings(settings.functionalitySettings)
          else FairPoSCalculator.V1
        else NxtPoSCalculator

      val hitSourceCache = TrieMap[(KeyPair, ByteStr), (BigInt, ByteStr)]()

      def getHitWithSource(account: KeyPair, hitSource: ByteStr): (BigInt, ByteStr) =
        hitSourceCache.getOrElseUpdate(
          (account, hitSource), {
            val gs =
              if (settings.preActivated(BlockchainFeatures.BlockV5)) {
                val vrfProof = crypto.signVRF(account.privateKey, hitSource.arr)
                crypto
                  .verifyVRF(vrfProof, hitSource.arr, account.publicKey, settings.preActivated(BlockchainFeatures.RideV6))
                  .map(_.arr)
                  .explicitGet()
              } else generationSignature(hitSource, account.publicKey)

            (hit(gs), ByteStr(gs))
          }
        )

      def inverseCalculateDelay(balance: Long, hitRate: Double): Int =
        posCalculator match {
          case FairPoSCalculator(minBlockTime, _) =>
            val averageBlockDelay = settings.averageBlockDelay.toMillis
            require(
              averageBlockDelay > minBlockTime,
              s"average-block-delay: ${averageBlockDelay}ms should be > min-block-time: ${minBlockTime}ms"
            )
            val z = (1 - Math.exp((averageBlockDelay - minBlockTime) / 70000.0)) * balance
            (5e17 * (Math.log(hitRate) / z)).toInt
          case NxtPoSCalculator =>
            (FairPoSCalculator.MaxHit * hitRate / settings.averageBlockDelay.toSeconds / balance).toInt
        }

      def nextBaseTarget(baseTarget: Long, height: Int, maybeGreatGrandParentTimestamp: Option[Long], parentTimestamp: Long, timestamp: Long): Share =
        posCalculator.calculateBaseTarget(
          settings.averageBlockDelay.toSeconds,
          height,
          baseTarget,
          parentTimestamp,
          maybeGreatGrandParentTimestamp,
          timestamp
        )

      def parallelMapMin[A, B, C](seq: Seq[A], f: A => B, fMin: B => C)(implicit c: Ordering[C]): B = {
        val partSize        = (seq.size / Runtime.getRuntime.availableProcessors()).max(1)
        val parallelResults = seq.grouped(partSize).map(part => Future(part.map(f).minBy(fMin)))
        Await.result(Future.sequence(parallelResults), Duration.Inf).minBy(fMin)
      }

      def calc(hitSources: Seq[ByteStr], timestamps: Seq[Long], baseTargets: Seq[Long], height: Int, n: Int): Seq[Long] =
        if (n == 0) baseTargets
        else {
          val currentHitSource = if (height > 100) hitSources(100) else hitSources.head
          val (delay, newHitSource) = parallelMapMin[(FullAddressInfo, Share), (Long, ByteStr), Long](
            minerShares,
            { case (miner, balance) =>
              val (hit, newHitSource) = getHitWithSource(miner.account, currentHitSource)
              val delay               = posCalculator.calculateDelay(hit, baseTargets.head, balance)
              (delay, newHitSource)
            },
            _._1
          )
          val newTimestamp  = timestamps.head + delay
          val newBaseTarget = nextBaseTarget(baseTargets.head, height, timestamps.lift(2), timestamps.head, newTimestamp)
          calc(
            newHitSource +: hitSources,
            newTimestamp +: timestamps,
            newBaseTarget +: baseTargets,
            height + 1,
            n - 1
          )
        }

      val startHitSource  = ByteStr(Array.fill(crypto.DigestLength)(0: Byte))
      val startBaseTarget = inverseCalculateDelay(minerShares.map(_._2).max, 0.5)

      val totalCount       = 1000
      val significantCount = 100

      val baseTargets = calc(Seq(startHitSource), Seq(0), Seq(startBaseTarget), 1, totalCount)
      baseTargets.take(significantCount).sum / significantCount
    }

    generateAndReport(
      addrInfos = shares.map(_._1),
      settings = genesisSettings(settings.baseTarget)
    )
  }
}
