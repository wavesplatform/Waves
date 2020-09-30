package com.wavesplatform

import java.io.{File, FileNotFoundException}
import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme, Alias, KeyPair}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.crypto._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings, GenesisTransactionSettings, RewardsSettings}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state._
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{Asset, GenesisTransaction, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.wallet.Wallet
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

import scala.annotation.tailrec
import scala.concurrent.duration._

object GenesisBlockGenerator extends App {

  private type AccountName = String
  private type SeedText    = String
  private type Share       = Long

  case class DistributionItem(seedText: String, nonce: Int, amount: Share)

  case class Settings(
      networkType: String,
      initialBalance: Share,
      baseTarget: Option[Long],
      averageBlockDelay: FiniteDuration,
      timestamp: Option[Long],
      distributions: Map[AccountName, DistributionItem],
      preActivatedFeatures: Option[List[Int]]
  ) {

    private[this] val distributionsSum = distributions.values.map(_.amount).sum
    require(
      distributionsSum == initialBalance,
      s"The sum of all balances should be == $initialBalance, but it is $distributionsSum"
    )

    val chainId: Byte = networkType.head.toByte

    def blockchainSettings(genesisSettings: GenesisSettings): BlockchainSettings =
      BlockchainSettings(
        networkType.head,
        FunctionalitySettings(
          Int.MaxValue,
          Int.MaxValue,
          preActivatedFeatures = preActivatedFeatures.getOrElse(List()).map(f => f.toShort -> 0).toMap,
          doubleFeaturesPeriodsAfterHeight = Int.MaxValue
        ),
        genesisSettings,
        RewardsSettings(Int.MaxValue, Long.MaxValue, Long.MaxValue, Int.MaxValue)
      )
  }

  case class FullAddressInfo(
      seedText: SeedText,
      seed: ByteStr,
      accountSeed: ByteStr,
      accountPrivateKey: ByteStr,
      accountPublicKey: ByteStr,
      accountAddress: Address,
      account: KeyPair
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
      acc
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
    override val chainId: Byte = settings.chainId
  }

  val shares: Seq[(AccountName, FullAddressInfo, Share)] = settings.distributions
    .map {
      case (accountName, x) => (accountName, toFullAddressInfo(x), x.amount)
    }
    .toSeq
    .sortBy(_._1)

  val timestamp = settings.timestamp.getOrElse(System.currentTimeMillis())

  val genesisTxs: Seq[GenesisTransaction] = shares.map {
    case (_, addrInfo, part) =>
      GenesisTransaction(addrInfo.accountAddress, part, timestamp, ByteStr.empty, settings.chainId)
  }

  report(
    addrInfos = shares.map(x => (x._1, x._2)),
    settings = genesisSettings(settings.baseTarget)
  )

  private def report(addrInfos: Iterable[(AccountName, FullAddressInfo)], settings: GenesisSettings): Unit = {
    val output = new StringBuilder(8192)
    output.append("Addresses:\n")
    addrInfos.foreach {
      case (accountName, acc) =>
        output.append(s"""$accountName:
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
    outputConfFile.foreach(ocf => Files.write(ocf.toPath, confBody.utf8Bytes))
  }

  def genesisSettings(predefined: Option[Long]): GenesisSettings = {
    predefined
      .map(baseTarget => mkGenesisSettings(mkGenesisBlock(baseTarget)))
      .getOrElse {
        @tailrec
        def loop(bt: Long): GenesisSettings = {
          checkBaseTarget(bt) match {
            case None          => loop(bt + 1)
            case Some(genesis) => mkGenesisSettings(genesis)
          }
        }
        loop(1)
      }
  }

  def mkGenesisBlock(baseTarget: Long): Block = {
    val reference     = ByteStr(Array.fill(SignatureLength)(-1: Byte))
    val genesisSigner = KeyPair(ByteStr.empty)
    Block
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
  }

  def mkGenesisSettings(genesis: Block): GenesisSettings =
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

  def checkBaseTarget(bt: Long): Option[Block] = {
    val genesis         = mkGenesisBlock(bt)
    val genesisSettings = mkGenesisSettings(genesis)

    val pos = PoSSelector(InitialBlockchain(genesis.header.generationSignature, settings.blockchainSettings(genesisSettings)), None)
    shares
      .map {
        case (_, accountInfo, amount) =>
          timestamp + pos.getValidBlockDelay(1, accountInfo.account, genesis.header.baseTarget, amount).explicitGet()
      }
      .find { nextGenTime =>
        timestamp + settings.averageBlockDelay.toMillis == nextGenTime
      }
      .map { nextGenTime =>
        System.out.println(s"Next generation time: $nextGenTime")
        genesis
      }
  }
}

case class InitialBlockchain(hitSource: ByteStr, settings: BlockchainSettings) extends Blockchain {
  def height: Int                                                                              = 1
  def score: BigInt                                                                            = 0
  def blockHeader(height: Int): Option[SignedBlockHeader]                                      = None
  def hitSource(height: Int): Option[ByteStr]                                                  = Some(hitSource)
  def carryFee: Long                                                                           = 0
  def heightOf(blockId: ByteStr): Option[Int]                                                  = None
  def approvedFeatures: Map[Short, Int]                                                        = Map()
  def activatedFeatures: Map[Short, Int]                                                       = settings.functionalitySettings.preActivatedFeatures
  def featureVotes(height: Int): Map[Short, Int]                                               = Map()
  def blockReward(height: Int): Option[Long]                                                   = None
  def blockRewardVotes(height: Int): Seq[Long]                                                 = Seq()
  def wavesAmount(height: Int): BigInt                                                         = 0
  def transferById(id: ByteStr): Option[(Int, TransferTransaction)]                            = None
  def transactionInfo(id: ByteStr): Option[(Int, Transaction, Boolean)]                        = None
  def transactionMeta(id: ByteStr): Option[(Int, Boolean)]                                     = None
  def containsTransaction(tx: Transaction): Boolean                                            = false
  def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription]                        = None
  def resolveAlias(a: Alias): Either[ValidationError, Address]                                 = Left(GenericError("Empty blockchain"))
  def leaseDetails(leaseId: ByteStr): Option[LeaseDetails]                                     = None
  def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee                                       = VolumeAndFee.empty
  def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)]      = None
  def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = Seq()
  def accountScript(address: Address): Option[AccountScriptInfo]                               = None
  def hasAccountScript(address: Address): Boolean                                              = false
  def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo]                              = None
  def accountData(acc: Address, key: String): Option[DataEntry[_]]                             = None
  def leaseBalance(address: Address): LeaseBalance                                             = LeaseBalance.empty
  def balance(address: Address, mayBeAssetId: Asset): Long                                     = 0
}
