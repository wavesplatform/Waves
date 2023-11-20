package com.wavesplatform.state

import cats.data.Ior
import cats.implicits.{catsSyntaxEitherId, catsSyntaxSemigroup, toBifunctorOps, toTraverseOps}
import cats.kernel.Monoid
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.{Asset, Transaction}

import scala.collection.immutable.VectorMap

case class StateSnapshot(
    transactions: VectorMap[ByteStr, NewTransactionInfo] = VectorMap(),
    balances: VectorMap[(Address, Asset), Long] = VectorMap(),
    leaseBalances: Map[Address, LeaseBalance] = Map(),
    assetStatics: VectorMap[IssuedAsset, AssetStaticInfo] = VectorMap(),
    assetVolumes: Map[IssuedAsset, AssetVolumeInfo] = Map(),
    assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo] = Map(),
    assetScripts: Map[IssuedAsset, AssetScriptInfo] = Map(),
    sponsorships: Map[IssuedAsset, SponsorshipValue] = Map(),
    newLeases: Map[ByteStr, LeaseStaticInfo] = Map(),
    cancelledLeases: Map[ByteStr, LeaseDetails.Status.Inactive] = Map.empty,
    aliases: Map[Alias, Address] = Map(),
    orderFills: Map[ByteStr, VolumeAndFee] = Map(),
    accountScripts: Map[PublicKey, Option[AccountScriptInfo]] = Map(),
    accountData: Map[Address, Map[String, DataEntry[?]]] = Map(),
    scriptResults: Map[ByteStr, InvokeScriptResult] = Map(),
    ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] = Map(),
    scriptsComplexity: Long = 0
) {

  // ignores lease balances from portfolios
  def addBalances(portfolios: Map[Address, Portfolio], blockchain: Blockchain): Either[String, StateSnapshot] =
    StateSnapshot
      .balances(portfolios, SnapshotBlockchain(blockchain, this))
      .map(b => copy(balances = balances ++ b))

  def withTransaction(tx: NewTransactionInfo): StateSnapshot =
    copy(transactions + (tx.transaction.id() -> tx))

  def addScriptsComplexity(scriptsComplexity: Long): StateSnapshot =
    copy(scriptsComplexity = this.scriptsComplexity + scriptsComplexity)

  def setScriptsComplexity(newScriptsComplexity: Long): StateSnapshot =
    copy(scriptsComplexity = newScriptsComplexity)

  def setScriptResults(newScriptResults: Map[ByteStr, InvokeScriptResult]): StateSnapshot =
    copy(scriptResults = newScriptResults)

  def errorMessage(txId: ByteStr): Option[InvokeScriptResult.ErrorMessage] =
    scriptResults.get(txId).flatMap(_.error)

  def bindElidedTransaction(blockchain: Blockchain, tx: Transaction): StateSnapshot =
    copy(
      transactions = transactions + (tx.id() -> NewTransactionInfo.create(tx, TxMeta.Status.Elided, StateSnapshot.empty, blockchain))
    )

  lazy val indexedAssetStatics: Map[IssuedAsset, (AssetStaticInfo, Int)] =
    assetStatics.zipWithIndex.map { case ((asset, static), i) => asset -> (static, i + 1) }.toMap

  lazy val accountScriptsByAddress: Map[Address, Option[AccountScriptInfo]] =
    accountScripts.map { case (pk, script) => (pk.toAddress, script) }

  lazy val hashString: String =
    Integer.toHexString(hashCode())
}

object StateSnapshot {

  def build(
      blockchain: Blockchain,
      portfolios: Map[Address, Portfolio] = Map(),
      orderFills: Map[ByteStr, VolumeAndFee] = Map(),
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] = VectorMap(),
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] = Map(),
      assetScripts: Map[IssuedAsset, AssetScriptInfo] = Map(),
      sponsorships: Map[IssuedAsset, Sponsorship] = Map(),
      newLeases: Map[ByteStr, LeaseStaticInfo] = Map(),
      cancelledLeases: Map[ByteStr, LeaseDetails.Status.Inactive] = Map.empty,
      aliases: Map[Alias, Address] = Map(),
      accountData: Map[Address, Map[String, DataEntry[?]]] = Map(),
      accountScripts: Map[PublicKey, Option[AccountScriptInfo]] = Map(),
      scriptResults: Map[ByteStr, InvokeScriptResult] = Map(),
      ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] = Map(),
      scriptsComplexity: Long = 0,
      transactions: VectorMap[ByteStr, NewTransactionInfo] = VectorMap()
  ): Either[ValidationError, StateSnapshot] = {
    val r =
      for {
        b  <- balances(portfolios, blockchain)
        lb <- leaseBalances(portfolios, blockchain)
        of <- this.orderFills(orderFills, blockchain)
      } yield StateSnapshot(
        transactions,
        b,
        lb,
        assetStatics(issuedAssets),
        assetVolumes(blockchain, issuedAssets, updatedAssets),
        assetNamesAndDescriptions(issuedAssets, updatedAssets),
        assetScripts,
        sponsorships.collect { case (asset, value: SponsorshipValue) => (asset, value) },
        newLeases,
        cancelledLeases,
        aliases,
        of,
        accountScripts,
        accountData,
        scriptResults,
        ethereumTransactionMeta,
        scriptsComplexity
      )
    r.leftMap(GenericError(_))
  }

  // ignores lease balances from portfolios
  private def balances(portfolios: Map[Address, Portfolio], blockchain: Blockchain): Either[String, VectorMap[(Address, Asset), Long]] =
    flatTraverse(portfolios) { case (address, Portfolio(wavesAmount, _, assets)) =>
      val assetBalancesE = flatTraverse(assets) {
        case (_, 0) =>
          Right(VectorMap[(Address, Asset), Long]())
        case (assetId, balance) =>
          safeSum(blockchain.balance(address, assetId), balance, s"$address -> Asset balance")
            .map(newBalance => VectorMap((address, assetId: Asset) -> newBalance))
      }
      if (wavesAmount != 0)
        for {
          assetBalances   <- assetBalancesE
          newWavesBalance <- safeSum(blockchain.balance(address), wavesAmount, s"$address -> Waves balance")
        } yield assetBalances + ((address, Waves) -> newWavesBalance)
      else
        assetBalancesE
    }

  private def flatTraverse[E, K1, V1, K2, V2](m: Map[K1, V1])(f: (K1, V1) => Either[E, VectorMap[K2, V2]]): Either[E, VectorMap[K2, V2]] =
    m.foldLeft(VectorMap[K2, V2]().asRight[E]) {
      case (e @ Left(_), _) =>
        e
      case (Right(acc), (k, v)) =>
        f(k, v).map(acc ++ _)
    }

  def ofLeaseBalances(balances: Map[Address, LeaseBalance], blockchain: Blockchain): Either[String, StateSnapshot] =
    balances.toSeq
      .traverse { case (address, leaseBalance) =>
        leaseBalance.combineF[Either[String, *]](blockchain.leaseBalance(address)).map(address -> _)
      }
      .map(newBalances => StateSnapshot(leaseBalances = newBalances.toMap))

  private def leaseBalances(portfolios: Map[Address, Portfolio], blockchain: Blockchain): Either[String, Map[Address, LeaseBalance]] =
    portfolios.toSeq
      .flatTraverse {
        case (address, Portfolio(_, lease, _)) if lease.out != 0 || lease.in != 0 =>
          val bLease = blockchain.leaseBalance(address)
          for {
            newIn  <- safeSum(bLease.in, lease.in, s"$address -> Lease")
            newOut <- safeSum(bLease.out, lease.out, s"$address -> Lease")
          } yield Seq(address -> LeaseBalance(newIn, newOut))
        case _ =>
          Seq().asRight[String]
      }
      .map(_.toMap)

  private def assetStatics(issuedAssets: VectorMap[IssuedAsset, NewAssetInfo]): VectorMap[IssuedAsset, AssetStaticInfo] =
    issuedAssets.map { case (asset, info) =>
      asset -> info.static
    }

  private def assetVolumes(
      blockchain: Blockchain,
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo],
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]]
  ): Map[IssuedAsset, AssetVolumeInfo] = {
    val issued = issuedAssets.view.mapValues(_.volume).toMap
    val updated = updatedAssets.collect {
      case (asset, Ior.Right(volume))   => (asset, volume)
      case (asset, Ior.Both(_, volume)) => (asset, volume)
    }
    (issued |+| updated).map { case (asset, volume) =>
      val blockchainAsset = blockchain.assetDescription(asset)
      val newIsReissuable = blockchainAsset.map(_.reissuable && volume.isReissuable).getOrElse(volume.isReissuable)
      val newVolume       = blockchainAsset.map(_.totalVolume + volume.volume).getOrElse(volume.volume)
      asset -> AssetVolumeInfo(newIsReissuable, newVolume)
    }
  }

  private def assetNamesAndDescriptions(
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo],
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]]
  ): Map[IssuedAsset, AssetInfo] = {
    val issued = issuedAssets.view.mapValues(_.dynamic).toMap
    val updated = updatedAssets.collect {
      case (asset, Ior.Left(info))    => (asset, info)
      case (asset, Ior.Both(info, _)) => (asset, info)
    }
    issued ++ updated
  }

  private def orderFills(volumeAndFees: Map[ByteStr, VolumeAndFee], blockchain: Blockchain): Either[String, Map[ByteStr, VolumeAndFee]] =
    volumeAndFees.toSeq
      .traverse { case (orderId, value) =>
        value.combineE(blockchain.filledVolumeAndFee(orderId)).map(orderId -> _)
      }
      .map(_.toMap)

  implicit val monoid: Monoid[StateSnapshot] = new Monoid[StateSnapshot] {
    override val empty: StateSnapshot =
      StateSnapshot()

    override def combine(s1: StateSnapshot, s2: StateSnapshot): StateSnapshot =
      StateSnapshot(
        s1.transactions ++ s2.transactions,
        s1.balances ++ s2.balances,
        s1.leaseBalances ++ s2.leaseBalances,
        s1.assetStatics ++ s2.assetStatics,
        s1.assetVolumes ++ s2.assetVolumes,
        s1.assetNamesAndDescriptions ++ s2.assetNamesAndDescriptions,
        s1.assetScripts ++ s2.assetScripts,
        s1.sponsorships ++ s2.sponsorships,
        s1.newLeases ++ s2.newLeases,
        s1.cancelledLeases ++ s2.cancelledLeases,
        s1.aliases ++ s2.aliases,
        s1.orderFills ++ s2.orderFills,
        s1.accountScripts ++ s2.accountScripts,
        combineDataEntries(s1.accountData, s2.accountData),
        s1.scriptResults |+| s2.scriptResults,
        s1.ethereumTransactionMeta ++ s2.ethereumTransactionMeta,
        s1.scriptsComplexity + s2.scriptsComplexity
      )

    private def combineDataEntries(
        entries1: Map[Address, Map[String, DataEntry[?]]],
        entries2: Map[Address, Map[String, DataEntry[?]]]
    ): Map[Address, Map[String, DataEntry[?]]] =
      entries2.foldLeft(entries1) { case (result, (address, addressEntries2)) =>
        val resultAddressEntries =
          result
            .get(address)
            .fold(address -> addressEntries2)(addressEntries1 => address -> (addressEntries1 ++ addressEntries2))
        result + resultAddressEntries
      }
  }

  val empty: StateSnapshot = StateSnapshot()
}
