package com.wavesplatform.state

import cats.Monad
import cats.data.Ior
import cats.implicits.{catsSyntaxSemigroup, toFlatMapOps, toFunctorOps}
import cats.kernel.{Monoid, Semigroup}
import cats.syntax.either.*
import com.google.common.hash.{BloomFilter, Funnels}
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.{
  BurnTransaction,
  IssueTransaction,
  ReissueTransaction,
  SetAssetScriptTransaction,
  SponsorFeeTransaction,
  UpdateAssetInfoTransaction
}
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, InvokeTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer.{MassTransferTransaction, TransferTransaction}
import com.wavesplatform.transaction.{Asset, CreateAliasTransaction, DataTransaction, EthereumTransaction, Transaction}

import scala.collection.immutable.VectorMap
import scala.util.chaining.*

case class LeaseBalance(in: Long, out: Long) {
  def combineF[F[_]: Monad](that: LeaseBalance)(implicit s: Summarizer[F]): F[LeaseBalance] =
    for {
      in  <- s.sum(in, that.in, "Lease in")
      out <- s.sum(out, that.out, "Lease out")
    } yield LeaseBalance(in, out)
}

object LeaseBalance {
  val empty: LeaseBalance = LeaseBalance(0, 0)
}

case class VolumeAndFee(volume: Long, fee: Long)

object VolumeAndFee {
  val empty: VolumeAndFee = VolumeAndFee(0, 0)

  implicit val m: Monoid[VolumeAndFee] = new Monoid[VolumeAndFee] {
    override def empty: VolumeAndFee = VolumeAndFee.empty

    override def combine(x: VolumeAndFee, y: VolumeAndFee): VolumeAndFee =
      VolumeAndFee(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AssetInfo(name: ByteString, description: ByteString, lastUpdatedAt: Height)

object AssetInfo {
  implicit val semigroup: Semigroup[AssetInfo] = (_, y) => y

  def apply(name: String, description: String, lastUpdatedAt: Height): AssetInfo =
    AssetInfo(ByteString.copyFromUtf8(name), ByteString.copyFromUtf8(description), lastUpdatedAt)
}

case class AssetStaticInfo(id: ByteStr, source: TransactionId, issuer: PublicKey, decimals: Int, nft: Boolean)

case class AssetVolumeInfo(isReissuable: Boolean, volume: BigInt)
object AssetVolumeInfo {
  implicit val assetInfoMonoid: Monoid[AssetVolumeInfo] = new Monoid[AssetVolumeInfo] {
    override def empty: AssetVolumeInfo = AssetVolumeInfo(isReissuable = true, 0)
    override def combine(x: AssetVolumeInfo, y: AssetVolumeInfo): AssetVolumeInfo =
      AssetVolumeInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}

case class AssetScriptInfo(script: Script, complexity: Long)

case class AssetDescription(
    originTransactionId: ByteStr,
    issuer: PublicKey,
    name: ByteString,
    description: ByteString,
    decimals: Int,
    reissuable: Boolean,
    totalVolume: BigInt,
    lastUpdatedAt: Height,
    script: Option[AssetScriptInfo],
    sponsorship: Long,
    nft: Boolean,
    sequenceInBlock: Int,
    issueHeight: Height
)

sealed abstract class Sponsorship
case class SponsorshipValue(minFee: Long) extends Sponsorship
case object SponsorshipNoInfo             extends Sponsorship

object Sponsorship {
  implicit val sponsorshipMonoid: Monoid[Sponsorship] = new Monoid[Sponsorship] {
    override def empty: Sponsorship = SponsorshipNoInfo

    override def combine(x: Sponsorship, y: Sponsorship): Sponsorship = y match {
      case SponsorshipNoInfo => x
      case _                 => y
    }
  }

  def calcWavesFeeAmount(tx: Transaction, getSponsorship: IssuedAsset => Option[Long]): Long = tx.assetFee match {
    case (asset @ IssuedAsset(_), amountInAsset) =>
      val sponsorship = getSponsorship(asset).getOrElse(0L)
      Sponsorship.toWaves(amountInAsset, sponsorship)

    case (Asset.Waves, amountInWaves) =>
      amountInWaves
  }

  def sponsoredFeesSwitchHeight(blockchain: Blockchain): Int =
    blockchain
      .featureActivationHeight(BlockchainFeatures.FeeSponsorship.id)
      .map(h => h + blockchain.settings.functionalitySettings.activationWindowSize(h))
      .getOrElse(Int.MaxValue)

  def toWaves(assetFee: Long, sponsorship: Long): Long =
    if (sponsorship == 0) Long.MaxValue
    else {
      val waves = BigInt(assetFee) * FeeValidation.FeeUnit / sponsorship
      waves.bigInteger.longValueExact()
    }

  def fromWaves(wavesFee: Long, sponsorship: Long): Long =
    if (wavesFee == 0 || sponsorship == 0) 0
    else {
      val assetFee = BigInt(wavesFee) * sponsorship / FeeValidation.FeeUnit
      assetFee.bigInteger.longValueExact()
    }
}

case class NewTransactionInfo(transaction: Transaction, affected: Set[Address], status: Status, spentComplexity: Long)

case class NewAssetInfo(static: AssetStaticInfo, dynamic: AssetInfo, volume: AssetVolumeInfo)

case class LeaseActionInfo(invokeId: ByteStr, dAppPublicKey: PublicKey, recipient: AddressOrAlias, amount: Long)

case class Diff private (
    transactions: Vector[NewTransactionInfo],
    portfolios: Map[Address, Portfolio],
    issuedAssets: VectorMap[IssuedAsset, NewAssetInfo],
    updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
    aliases: Map[Alias, Address],
    orderFills: Map[ByteStr, VolumeAndFee],
    leaseState: Map[ByteStr, LeaseDetails],
    scripts: Map[Address, Option[AccountScriptInfo]],
    assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
    accountData: Map[Address, Map[String, DataEntry[?]]],
    sponsorship: Map[IssuedAsset, Sponsorship],
    scriptsRun: Int,
    scriptsComplexity: Long,
    scriptResults: Map[ByteStr, InvokeScriptResult],
    ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta],
    transactionFilter: Option[BloomFilter[Array[Byte]]]
) {
  @inline
  final def combineE(newer: Diff): Either[ValidationError, Diff] = combineF(newer).leftMap(GenericError(_))

  def containsTransaction(txId: ByteStr): Boolean =
    transactions.nonEmpty && transactionFilter.exists(_.mightContain(txId.arr)) && transactions.exists(_.transaction.id() == txId)

  def transaction(txId: ByteStr): Option[NewTransactionInfo] =
    if (transactions.nonEmpty && transactionFilter.exists(_.mightContain(txId.arr)))
      transactions.find(_.transaction.id() == txId)
    else None

  def withScriptsComplexity(newScriptsComplexity: Long): Diff = copy(scriptsComplexity = newScriptsComplexity)

  def withScriptResults(newScriptResults: Map[ByteStr, InvokeScriptResult]): Diff = copy(scriptResults = newScriptResults)

  def withScriptRuns(newScriptRuns: Int): Diff = copy(scriptsRun = newScriptRuns)

  def withPortfolios(newPortfolios: Map[Address, Portfolio]): Diff = copy(portfolios = newPortfolios)

  def combineF(newer: Diff): Either[String, Diff] =
    Diff
      .combine(portfolios, newer.portfolios)
      .map { portfolios =>
        val newTransactions = if (transactions.isEmpty) newer.transactions else transactions ++ newer.transactions
        val newFilter = transactionFilter match {
          case Some(bf) =>
            newer.transactions.foreach(nti => bf.put(nti.transaction.id().arr))
            Some(bf)
          case None => newer.transactionFilter
        }

        Diff(
          transactions = newTransactions,
          portfolios = portfolios,
          issuedAssets = issuedAssets ++ newer.issuedAssets,
          updatedAssets = updatedAssets |+| newer.updatedAssets,
          aliases = aliases ++ newer.aliases,
          orderFills = orderFills.combine(newer.orderFills),
          leaseState = leaseState ++ newer.leaseState,
          scripts = scripts ++ newer.scripts,
          assetScripts = assetScripts ++ newer.assetScripts,
          accountData = Diff.combine(accountData, newer.accountData),
          sponsorship = sponsorship.combine(newer.sponsorship),
          scriptsRun = scriptsRun + newer.scriptsRun,
          scriptResults = scriptResults.combine(newer.scriptResults),
          scriptsComplexity = scriptsComplexity + newer.scriptsComplexity,
          ethereumTransactionMeta = ethereumTransactionMeta ++ newer.ethereumTransactionMeta,
          transactionFilter = newFilter
        )
      }
}

object Diff {
  def apply(
      portfolios: Map[Address, Portfolio] = Map.empty,
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] = VectorMap.empty,
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] = Map.empty,
      aliases: Map[Alias, Address] = Map.empty,
      orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
      leaseState: Map[ByteStr, LeaseDetails] = Map.empty,
      scripts: Map[Address, Option[AccountScriptInfo]] = Map.empty,
      assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] = Map.empty,
      accountData: Map[Address, Map[String, DataEntry[?]]] = Map.empty,
      sponsorship: Map[IssuedAsset, Sponsorship] = Map.empty,
      scriptsRun: Int = 0,
      scriptsComplexity: Long = 0,
      scriptResults: Map[ByteStr, InvokeScriptResult] = Map.empty,
      ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] = Map.empty
  ): Diff =
    new Diff(
      Vector.empty,
      portfolios,
      issuedAssets,
      updatedAssets,
      aliases,
      orderFills,
      leaseState,
      scripts,
      assetScripts,
      accountData,
      sponsorship,
      scriptsRun,
      scriptsComplexity,
      scriptResults,
      ethereumTransactionMeta,
      None
    )

  def withTransactions(
      nti: Vector[NewTransactionInfo],
      portfolios: Map[Address, Portfolio] = Map.empty,
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] = VectorMap.empty,
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] = Map.empty,
      aliases: Map[Alias, Address] = Map.empty,
      orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
      leaseState: Map[ByteStr, LeaseDetails] = Map.empty,
      scripts: Map[Address, Option[AccountScriptInfo]] = Map.empty,
      assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] = Map.empty,
      accountData: Map[Address, Map[String, DataEntry[?]]] = Map.empty,
      sponsorship: Map[IssuedAsset, Sponsorship] = Map.empty,
      scriptsRun: Int = 0,
      scriptsComplexity: Long = 0,
      scriptResults: Map[ByteStr, InvokeScriptResult] = Map.empty,
      ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] = Map.empty
  ): Diff =
    new Diff(
      nti,
      portfolios,
      issuedAssets,
      updatedAssets,
      aliases,
      orderFills,
      leaseState,
      scripts,
      assetScripts,
      accountData,
      sponsorship,
      scriptsRun,
      scriptsComplexity,
      scriptResults,
      ethereumTransactionMeta,
      mkFilterForTransactions(nti.map(_.transaction)*)
    )

  val empty: Diff = Diff()

  def combine(portfolios1: Map[Address, Portfolio], portfolios2: Map[Address, Portfolio]): Either[String, Map[Address, Portfolio]] =
    if (portfolios1.isEmpty) Right(portfolios2)
    else if (portfolios2.isEmpty) Right(portfolios1)
    else
      portfolios2.foldLeft[Either[String, Map[Address, Portfolio]]](Right(portfolios1)) {
        case (Right(seed), kv @ (address, pf)) =>
          seed.get(address).fold[Either[String, Map[Address, Portfolio]]](Right(seed + kv)) { oldPf =>
            oldPf
              .combine(pf)
              .bimap(
                err => s"$address: " + err,
                newPf => seed + (address -> newPf)
              )
          }
        case (r, _) => r
      }

  def combine[K, IK, IV](first: Map[K, Map[IK, IV]], second: Map[K, Map[IK, IV]]): Map[K, Map[IK, IV]] = {
    if (first.isEmpty) {
      second
    } else {
      first ++ second.map { case (k, innerMap) =>
        k -> first.get(k).fold(innerMap)(_ ++ innerMap)
      }
    }
  }

  private def mkFilter() =
    BloomFilter.create[Array[Byte]](Funnels.byteArrayFunnel(), 10000, 0.01f)
  private def mkFilterForTransactions(tx: Transaction*) =
    Some(
      mkFilter().tap(bf =>
        tx.foreach { t =>
          bf.put(t.id().arr)
        }
      )
    )

  implicit class DiffExt(private val d: Diff) extends AnyVal {
    def errorMessage(txId: ByteStr): Option[InvokeScriptResult.ErrorMessage] =
      d.scriptResults.get(txId).flatMap(_.error)

    def hashString: String =
      Integer.toHexString(d.hashCode())

    def bindTransaction(blockchain: Blockchain, tx: Transaction): Diff = {
      val calledScripts     = d.scriptResults.values.flatMap(inv => InvokeScriptResult.Invocation.calledAddresses(inv.invokes))
      val maybeDApp         = extractDAppAddress(blockchain, tx)
      val affectedAddresses = d.portfolios.keySet ++ d.accountData.keySet ++ calledScripts ++ maybeDApp

      d.copy(
        transactions = Vector(NewTransactionInfo(tx, affectedAddresses, TxMeta.Status.Succeeded, d.scriptsComplexity)),
        transactionFilter = mkFilterForTransactions(tx)
      )
    }

    def bindElidedTransaction(blockchain: Blockchain, tx: Transaction): Diff = {
      val maybeDApp = extractDAppAddress(blockchain, tx)
      val specificAffectedAddresses = tx match {
        case t: BurnTransaction        => Set(t.sender.toAddress)
        case t: CreateAliasTransaction => Set(t.sender.toAddress)
        case t: DataTransaction        => Set(t.sender.toAddress)
        case t: EthereumTransaction =>
          Set(t.sender.toAddress) ++
            (t.payload match {
              case EthereumTransaction.Transfer(_, _, recipient) => Set(recipient)
              case _                                             => Set.empty
            })
        case t: ExchangeTransaction     => Set(t.sender.toAddress, t.order1.sender.toAddress, t.order2.sender.toAddress)
        case t: InvokeScriptTransaction => Set(t.sender.toAddress)
        case t: IssueTransaction        => Set(t.sender.toAddress)
        case t: LeaseCancelTransaction =>
          Set(t.sender.toAddress) ++ blockchain
            .leaseDetails(t.leaseId)
            .flatMap(_.recipient match {
              case alias: Alias     => blockchain.resolveAlias(alias).toOption
              case address: Address => Some(address)
            })
            .toSet
        case t: LeaseTransaction =>
          Set(t.sender.toAddress) ++ (t.recipient match {
            case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
            case address: Address => Set(address)
          })
        case t: MassTransferTransaction =>
          Set(t.sender.toAddress) ++ (t.transfers.flatMap {
            _.address match {
              case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
              case address: Address => Set(address)
            }
          })
        case t: ReissueTransaction         => Set(t.sender.toAddress)
        case t: SetAssetScriptTransaction  => Set(t.sender.toAddress)
        case t: SetScriptTransaction       => Set(t.sender.toAddress)
        case t: SponsorFeeTransaction      => Set(t.sender.toAddress)
        case t: UpdateAssetInfoTransaction => Set(t.sender.toAddress)
        case t: TransferTransaction =>
          Set(t.sender.toAddress) ++ (t.recipient match {
            case alias: Alias     => blockchain.resolveAlias(alias).toOption.toSet
            case address: Address => Set(address)
          })
        case _ => Set.empty
      }
      val affectedAddresses = specificAffectedAddresses ++ maybeDApp

      val txFilter = d.transactionFilter.getOrElse(mkFilter())
      txFilter.put(tx.id().arr)
      d.copy(
        transactions = d.transactions :+ NewTransactionInfo(tx, affectedAddresses, TxMeta.Status.Elided, d.scriptsComplexity),
        transactionFilter = Some(txFilter)
      )
    }

    private def extractDAppAddress(blockchain: Blockchain, tx: Transaction): Option[Address] = {
      tx match {
        case i: InvokeTransaction =>
          i.dApp match {
            case alias: Alias     => d.aliases.get(alias).orElse(blockchain.resolveAlias(alias).toOption)
            case address: Address => Some(address)
          }
        case et: EthereumTransaction =>
          et.payload match {
            case EthereumTransaction.Invocation(dApp, _) => Some(dApp)
            case _                                       => None
          }
        case _ =>
          None
      }
    }
  }
}
