package com.wavesplatform.state
import cats.data.Ior
import cats.implicits.{catsSyntaxEitherId, toBifunctorOps, toTraverseOps}
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.{AssetStatic, TransactionStatus}
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt, ByteStringExt}
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.state.reader.{LeaseDetails, SnapshotBlockchain}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.GenericError

import scala.collection.immutable.VectorMap

case class StateSnapshot(
    transactions: VectorMap[ByteStr, NewTransactionInfo] = VectorMap(),
    balances: VectorMap[(Address, Asset), Long] = VectorMap(),
    leaseBalances: Map[Address, LeaseBalance] = Map(),
    assetStatics: Map[IssuedAsset, AssetStatic] = Map(),
    assetVolumes: Map[IssuedAsset, AssetVolumeInfo] = Map(),
    assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo] = Map(),
    assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] = Map(),
    sponsorships: Map[IssuedAsset, SponsorshipValue] = Map(),
    leaseStates: Map[ByteStr, LeaseDetails] = Map(),
    aliases: Map[Alias, Address] = Map(),
    orderFills: Map[ByteStr, VolumeAndFee] = Map(),
    accountScripts: Map[Address, Option[AccountScriptInfo]] = Map(),
    accountData: Map[Address, Map[String, DataEntry[?]]] = Map(),
    scriptResults: Map[ByteStr, InvokeScriptResult] = Map(),
    ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] = Map(),
    scriptsComplexity: Long = 0
) {
  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S

  def toProtobuf(txSucceeded: Boolean): TransactionStateSnapshot =
    TransactionStateSnapshot(
      balances.map { case ((address, asset), balance) =>
        S.Balance(address.toByteString, Some(Amount(asset.fold(ByteString.EMPTY)(_.id.toByteString), balance)))
      }.toSeq,
      leaseBalances.map { case (address, balance) =>
        S.LeaseBalance(address.toByteString, balance.in, balance.out)
      }.toSeq,
      assetStatics.values.toSeq,
      assetVolumes.map { case (asset, info) =>
        S.AssetVolume(asset.id.toByteString, info.isReissuable, ByteString.copyFrom(info.volume.toByteArray))
      }.toSeq,
      assetNamesAndDescriptions.map { case (asset, info) =>
        S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
      }.toSeq,
      assetScripts.map { case (asset, script) =>
        S.AssetScript(asset.id.toByteString, script.fold(ByteString.EMPTY)(_.script.bytes().toByteString), script.fold(0L)(_.complexity))
      }.toSeq,
      aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.toSeq,
      orderFills.map { case (orderId, VolumeAndFee(volume, fee)) =>
        S.OrderFill(orderId.toByteString, volume, fee)
      }.toSeq,
      leaseStates.map { case (leaseId, LeaseDetails(sender, recipient, amount, status, sourceId, height)) =>
        val pbStatus = status match {
          case Status.Active =>
            S.LeaseState.Status.Active(S.LeaseState.Active())
          case Status.Cancelled(cancelHeight, txId) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(cancelHeight, txId.fold(ByteString.EMPTY)(_.toByteString)))
          case Status.Expired(expiredHeight) =>
            S.LeaseState.Status.Cancelled(S.LeaseState.Cancelled(expiredHeight))
        }
        S.LeaseState(
          leaseId.toByteString,
          pbStatus,
          amount,
          sender.toByteString,
          ByteString.copyFrom(recipient.asInstanceOf[Address].bytes),
          sourceId.toByteString,
          height
        )
      }.toSeq,
      accountScripts.map { case (_, scriptOpt) =>
        scriptOpt.fold(
          S.AccountScript()
        )(script =>
          S.AccountScript(
            script.publicKey.toByteString,
            script.script.bytes().toByteString,
            script.verifierComplexity
          )
        )
      }.toSeq,
      accountData.map { case (address, data) =>
        S.AccountData(address.toByteString, data.values.map(PBTransactions.toPBDataEntry).toSeq)
      }.toSeq,
      sponsorships.collect { case (asset, SponsorshipValue(minFee)) =>
        S.Sponsorship(asset.id.toByteString, minFee)
      }.toSeq,
      transactionStatus = if (txSucceeded) TransactionStatus.SUCCEEDED else TransactionStatus.FAILED
    )

  def addBalances(portfolios: Map[Address, Portfolio], blockchain: Blockchain): Either[ValidationError, StateSnapshot] =
    StateSnapshot
      .balances(portfolios, SnapshotBlockchain(blockchain, this))
      .leftMap(GenericError(_))
      .map(b => copy(balances = balances ++ b))

  def withTransaction(tx: NewTransactionInfo): StateSnapshot =
    copy(transactions + (tx.transaction.id() -> tx))

  def errorMessage(txId: ByteStr): Option[InvokeScriptResult.ErrorMessage] =
    scriptResults.get(txId).flatMap(_.error)

  lazy val indexedAssetStatics: Map[IssuedAsset, (AssetStatic, Int)] =
    assetStatics.zipWithIndex.map { case ((asset, static), i) => asset -> (static, i + 1) }.toMap

  lazy val hashString: String =
    Integer.toHexString(hashCode())
}

object StateSnapshot {
  def fromProtobuf(pbSnapshot: TransactionStateSnapshot): (StateSnapshot, Boolean) = {
    val balances: VectorMap[(Address, Asset), Long] =
      VectorMap() ++ pbSnapshot.balances.map(b => (b.address.toAddress, b.getAmount.assetId.toAssetId) -> b.getAmount.amount)

    val leaseBalances: Map[Address, LeaseBalance] =
      pbSnapshot.leaseBalances
        .map(b => b.address.toAddress -> LeaseBalance(b.in, b.out))
        .toMap

    val assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] =
      pbSnapshot.assetScripts.map { s =>
        val info =
          if (s.script.isEmpty)
            None
          else
            Some(AssetScriptInfo(ScriptReader.fromBytes(s.script.toByteArray).explicitGet(), s.complexity))
        s.assetId.toIssuedAssetId -> info
      }.toMap

    val assetStatics: Map[IssuedAsset, AssetStatic] =
      pbSnapshot.assetStatics.map(info => info.assetId.toIssuedAssetId -> info).toMap

    val assetVolumes: Map[IssuedAsset, AssetVolumeInfo] =
      pbSnapshot.assetVolumes
        .map(v => v.assetId.toIssuedAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray)))
        .toMap

    val assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo] =
      pbSnapshot.assetNamesAndDescriptions
        .map(i => i.assetId.toIssuedAssetId -> AssetInfo(i.name, i.description, Height @@ i.lastUpdated))
        .toMap

    val sponsorships: Map[IssuedAsset, SponsorshipValue] =
      pbSnapshot.sponsorships
        .map(s => s.assetId.toIssuedAssetId -> SponsorshipValue(s.minFee))
        .toMap

    val leaseStates: Map[ByteStr, LeaseDetails] =
      pbSnapshot.leaseStates
        .map(ls =>
          ls.leaseId.toByteStr -> LeaseDetails(
            ls.sender.toPublicKey,
            PBRecipients.toAddress(ls.recipient.toByteArray, AddressScheme.current.chainId).explicitGet(),
            ls.amount,
            ls.status match {
              case TransactionStateSnapshot.LeaseState.Status.Cancelled(c) =>
                LeaseDetails.Status.Cancelled(c.height, if (c.transactionId.isEmpty) None else Some(c.transactionId.toByteStr))
              case _ =>
                LeaseDetails.Status.Active
            },
            ls.originTransactionId.toByteStr,
            ls.height
          )
        )
        .toMap

    val aliases: Map[Alias, Address] =
      pbSnapshot.aliases
        .map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress)
        .toMap

    val orderFills: Map[ByteStr, VolumeAndFee] =
      pbSnapshot.orderFills
        .map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee))
        .toMap

    val accountScripts: Map[Address, Option[AccountScriptInfo]] =
      pbSnapshot.accountScripts.map { pbInfo =>
        val info =
          if (pbInfo.script.isEmpty)
            None
          else
            Some(
              AccountScriptInfo(
                pbInfo.senderPublicKey.toPublicKey,
                ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(),
                pbInfo.verifierComplexity
              )
            )
        pbInfo.senderPublicKey.toAddress -> info
      }.toMap

    val accountData: Map[Address, Map[String, DataEntry[?]]] =
      pbSnapshot.accountData.map { data =>
        val entries =
          data.entries.map { pbEntry =>
            val entry = PBTransactions.toVanillaDataEntry(pbEntry)
            entry.key -> entry
          }.toMap
        data.address.toAddress -> entries
      }.toMap

    (
      StateSnapshot(
        VectorMap(),
        balances,
        leaseBalances,
        assetStatics,
        assetVolumes,
        assetNamesAndDescriptions,
        assetScripts,
        sponsorships,
        leaseStates,
        aliases,
        orderFills,
        accountScripts,
        accountData
      ),
      pbSnapshot.transactionStatus.isSucceeded
    )
  }

  def fromDiff(diff: Diff, blockchain: Blockchain): Either[ValidationError, StateSnapshot] =
    for {
      b <- balances(diff.portfolios, blockchain).leftMap(GenericError(_))
      lb <- leaseBalances(diff, blockchain).leftMap(GenericError(_))
    } yield StateSnapshot(
      VectorMap() ++ diff.transactions.map(info => info.transaction.id() -> info).toMap,
      b,
      lb,
      assetStatics(diff),
      assetVolumes(diff, blockchain),
      assetNamesAndDescriptions(diff),
      diff.assetScripts,
      diff.sponsorship.collect { case (asset, value: SponsorshipValue) => (asset, value) },
      resolvedLeaseStates(diff, blockchain),
      diff.aliases,
      orderFills(diff, blockchain),
      diff.scripts,
      diff.accountData,
      diff.scriptResults,
      diff.ethereumTransactionMeta,
      diff.scriptsComplexity
    )

  private def balances(portfolios: Map[Address, Portfolio], blockchain: Blockchain): Either[String, VectorMap[(Address, Asset), Long]] =
    flatTraverse(portfolios) { case (address, Portfolio(wavesAmount, _, assets)) =>
      val assetBalancesE = flatTraverse(assets) {
        case (_, 0) =>
          Right(VectorMap[(Address, Asset), Long]())
        case (assetId, balance) =>
          Portfolio
            .sum(blockchain.balance(address, assetId), balance, "Asset balance sum overflow")
            .map(newBalance => VectorMap((address, assetId: Asset) -> newBalance))
      }
      if (wavesAmount != 0)
        for {
          assetBalances   <- assetBalancesE
          newWavesBalance <- Portfolio.sum(blockchain.balance(address), wavesAmount, "Waves balance sum overflow")
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

  import cats.implicits.*

  private def leaseBalances(diff: Diff, blockchain: Blockchain): Either[String, Map[Address, LeaseBalance]] =
    diff.portfolios.toSeq
      .flatTraverse {
        case (address, Portfolio(_, lease, _)) if lease.out != 0 || lease.in != 0 =>
          val bLease = blockchain.leaseBalance(address)
          for {
            newIn  <- Portfolio.sum(bLease.in, lease.in, "Lease in overflow")
            newOut <- Portfolio.sum(bLease.out, lease.out, "Lease out overflow")
          } yield Seq(address -> LeaseBalance(newIn, newOut))
        case _ =>
          Seq().asRight[String]
      }
      .map(_.toMap)

  private def assetStatics(diff: Diff): Map[IssuedAsset, AssetStatic] =
    diff.issuedAssets.map { case (asset, info) =>
      asset ->
        AssetStatic(
          asset.id.toByteString,
          info.static.source.toByteString,
          info.static.issuer.toByteString,
          info.static.decimals,
          info.static.nft
        )
    }

  private def assetVolumes(diff: Diff, blockchain: Blockchain): Map[IssuedAsset, AssetVolumeInfo] = {
    val issued = diff.issuedAssets.view.mapValues(_.volume).toMap
    val updated = diff.updatedAssets.collect {
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

  private def assetNamesAndDescriptions(diff: Diff): Map[IssuedAsset, AssetInfo] = {
    val issued = diff.issuedAssets.view.mapValues(_.dynamic).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Left(info))    => (asset, info)
      case (asset, Ior.Both(info, _)) => (asset, info)
    }
    issued ++ updated
  }

  private def resolvedLeaseStates(diff: Diff, blockchain: Blockchain): Map[ByteStr, LeaseDetails] =
    diff.leaseState.view
      .mapValues(details =>
        details.copy(recipient = details.recipient match {
          case address: Address => address
          case alias: Alias     => diff.aliases.getOrElse(alias, blockchain.resolveAlias(alias).explicitGet())
        })
      )
      .toMap

  private def orderFills(diff: Diff, blockchain: Blockchain): Map[ByteStr, VolumeAndFee] =
    diff.orderFills.map { case (orderId, value) =>
      val newInfo = value |+| blockchain.filledVolumeAndFee(orderId)
      orderId -> newInfo
    }

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
        s1.leaseStates ++ s2.leaseStates,
        s1.aliases ++ s2.aliases,
        s1.orderFills ++ s2.orderFills,
        s1.accountScripts ++ s2.accountScripts,
        combineDataEntries(s1.accountData, s2.accountData),
        s1.scriptResults ++ s2.scriptResults,
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
