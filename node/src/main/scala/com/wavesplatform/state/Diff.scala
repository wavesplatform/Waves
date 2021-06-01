package com.wavesplatform.state

import scala.collection.immutable.VectorMap

import cats.data.Ior
import cats.implicits._
import cats.kernel.{Monoid, Semigroup}
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.diffs.FeeValidation
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.transaction.Asset.IssuedAsset

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty: LeaseBalance = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }
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

case class AssetStaticInfo(source: TransactionId, issuer: PublicKey, decimals: Int, nft: Boolean)

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
    nft: Boolean
)

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid: Monoid[AccountDataInfo] = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }

  implicit class AccountDataInfoExt(private val ad: AccountDataInfo) extends AnyVal {
    def filterEmpty: AccountDataInfo =
      ad.copy(ad.data.filterNot(_._2.isEmpty))
  }
}

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

case class NewTransactionInfo(transaction: Transaction, affected: Set[Address], applied: Boolean)

case class NewAssetInfo(static: AssetStaticInfo, dynamic: AssetInfo, volume: AssetVolumeInfo)

case class LeaseActionInfo(invokeId: ByteStr, dAppPublicKey: PublicKey, recipient: AddressOrAlias, amount: Long)

case class Diff(
    transactions: collection.Map[ByteStr, NewTransactionInfo] = VectorMap.empty,
    portfolios: Map[Address, Portfolio] = Map.empty,
    issuedAssets: Map[IssuedAsset, NewAssetInfo] = Map.empty,
    updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] = Map.empty,
    aliases: Map[Alias, Address] = Map.empty,
    orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
    leaseState: Map[ByteStr, LeaseDetails] = Map.empty,
    scripts: Map[Address, Option[AccountScriptInfo]] = Map.empty,
    assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] = Map.empty,
    accountData: Map[Address, AccountDataInfo] = Map.empty,
    sponsorship: Map[IssuedAsset, Sponsorship] = Map.empty,
    scriptsRun: Int = 0,
    scriptsComplexity: Long = 0,
    scriptResults: Map[ByteStr, InvokeScriptResult] = Map.empty
)

object Diff {
  val empty: Diff = Diff()

  implicit val diffMonoid: Monoid[Diff] = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        portfolios = older.portfolios.combine(newer.portfolios),
        issuedAssets = older.issuedAssets ++ newer.issuedAssets,
        updatedAssets = older.updatedAssets |+| newer.updatedAssets,
        aliases = older.aliases ++ newer.aliases,
        orderFills = older.orderFills.combine(newer.orderFills),
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        assetScripts = older.assetScripts ++ newer.assetScripts,
        accountData = older.accountData.combine(newer.accountData),
        sponsorship = older.sponsorship.combine(newer.sponsorship),
        scriptsRun = older.scriptsRun.combine(newer.scriptsRun),
        scriptResults = older.scriptResults.combine(newer.scriptResults),
        scriptsComplexity = older.scriptsComplexity + newer.scriptsComplexity
      )
  }

  implicit class DiffExt(private val d: Diff) extends AnyVal {
    def errorMessage(txId: ByteStr): Option[InvokeScriptResult.ErrorMessage] =
      d.scriptResults.get(txId).flatMap(_.error)

    def hashString: String =
      Integer.toHexString(d.hashCode())

    def bindTransaction(tx: Transaction): Diff = {
      val calledScripts = d.scriptResults.values
        .flatMap(inv => InvokeScriptResult.Invocation.calledAddresses(inv.invokes))

      val affectedAddresses = d.portfolios.keySet ++ d.accountData.keySet ++ calledScripts
      d.copy(transactions = VectorMap(tx.id() -> NewTransactionInfo(tx, affectedAddresses, applied = true)))
    }
  }
}
