package com.wavesplatform.state
import cats.data.Ior
import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.database.protobuf.EthereumTransactionMeta.Payload
import com.wavesplatform.lang.script.ScriptReader
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot.AssetStatic
import com.wavesplatform.protobuf.transaction.{PBRecipients, PBTransactions}
import com.wavesplatform.protobuf.{AddressExt, Amount, ByteStrExt, ByteStringExt}
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.reader.LeaseDetails.Status
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.immutable.VectorMap

case class StateSnapshot(transactions: Vector[NewTransactionInfo], current: TransactionStateSnapshot) {
  lazy val balances: Map[(Address, Asset), Long] =
    VectorMap() ++ current.balances.map(b => (b.address.toAddress, b.getAmount.assetId.toAssetId) -> b.getAmount.amount)

  lazy val leaseBalances: Map[Address, LeaseBalance] =
    current.leaseBalances
      .map(b => b.address.toAddress -> LeaseBalance(b.in, b.out))
      .toMap

  lazy val assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]] =
    current.assetScripts.map { s =>
      val info =
        if (s.script.isEmpty)
          None
        else
          Some(AssetScriptInfo(ScriptReader.fromBytes(s.script.toByteArray).explicitGet(), s.complexity))
      s.assetId.toIssuedAssetId -> info
    }.toMap

  lazy val issuedAssets: Map[IssuedAsset, (AssetStatic, Int)] =
    current.assetStatics.zipWithIndex.map { case (info, i) => info.assetId.toIssuedAssetId -> (info, i + 1) }.toMap

  lazy val assetVolumes: Map[IssuedAsset, AssetVolumeInfo] =
    current.assetVolumes
      .map(v => v.assetId.toIssuedAssetId -> AssetVolumeInfo(v.reissuable, BigInt(v.volume.toByteArray)))
      .toMap

  lazy val assetNamesAndDescriptions: Map[IssuedAsset, AssetInfo] =
    current.assetNamesAndDescriptions
      .map(i => i.assetId.toIssuedAssetId -> AssetInfo(i.name, i.description, Height @@ i.lastUpdated))
      .toMap

  lazy val sponsorships: Map[IssuedAsset, SponsorshipValue] =
    current.sponsorships
      .map(s => s.assetId.toIssuedAssetId -> SponsorshipValue(s.minFee))
      .toMap

  lazy val leaseStates: Map[ByteStr, LeaseDetails] =
    current.leaseStates
      .map(ls =>
        ls.leaseId.toByteStr -> LeaseDetails(
          ls.sender.toPublicKey,
          PBRecipients.toAddressOrAlias(ls.getRecipient, AddressScheme.current.chainId).explicitGet(),
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

  lazy val aliases: Map[Alias, Address] =
    current.aliases
      .map(a => Alias.create(a.alias).explicitGet() -> a.address.toAddress)
      .toMap

  lazy val orderFills: Map[ByteStr, VolumeAndFee] =
    current.orderFills
      .map(of => of.orderId.toByteStr -> VolumeAndFee(of.volume, of.fee))
      .toMap

  lazy val scripts: Map[Address, Option[AccountScriptInfo]] =
    current.accountScripts.map { pbInfo =>
      val info =
        if (pbInfo.script.isEmpty)
          None
        else
          Some(
            AccountScriptInfo(
              pbInfo.senderPublicKey.toPublicKey,
              ScriptReader.fromBytes(pbInfo.script.toByteArray).explicitGet(),
              pbInfo.verifierComplexity,
              if (pbInfo.callableComplexities.nonEmpty) Map(3 -> pbInfo.callableComplexities)
              else Map()
            )
          )
      pbInfo.senderAddress.toAddress -> info
    }.toMap

  lazy val accountData: Map[Address, Map[String, DataEntry[?]]] =
    current.accountData.map { data =>
      val entries =
        data.entry.map { pbEntry =>
          val entry = PBTransactions.toVanillaDataEntry(pbEntry)
          entry.key -> entry
        }.toMap
      data.address.toAddress -> entries
    }.toMap

  lazy val scriptResults: Map[ByteStr, InvokeScriptResult] =
    current.scriptResults.map { pbResult =>
      val txId   = pbResult.transactionId.toByteStr
      val result = InvokeScriptResult.fromPB(pbResult.getResult)
      txId -> result
    }.toMap

  lazy val ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta] =
    current.ethereumTransactionMeta.map { pbMeta =>
      import TransactionStateSnapshot.EthereumTransactionMeta as M
      val payload = pbMeta.payload match {
        case M.Payload.Empty =>
          Payload.Empty
        case M.Payload.Invocation(M.Invocation(functionCall, payments, _)) =>
          Payload.Invocation(EthereumTransactionMeta.Invocation(functionCall, payments))
        case M.Payload.Transfer(M.Transfer(publicKeyHash, amount, _)) =>
          Payload.Transfer(EthereumTransactionMeta.Transfer(publicKeyHash, amount))
      }
      pbMeta.transactionId.toByteStr -> EthereumTransactionMeta(payload)
    }.toMap

  val scriptsComplexity: Long =
    current.totalComplexity

  lazy val hashString: String =
    Integer.toHexString(hashCode())
}

object StateSnapshot {
  import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot as S
  private val lastEstimator = 3

  def create(diff: Diff, blockchain: Blockchain): StateSnapshot =
    StateSnapshot(
      diff.transactions,
      TransactionStateSnapshot(
        balances(diff, blockchain),
        leaseBalances(diff, blockchain),
        assetStatics(diff),
        assetVolumes(diff, blockchain),
        assetNamesAndDescriptions(diff),
        assetScripts(diff),
        aliases(diff),
        orderFills(diff, blockchain),
        leaseStates(diff),
        accountScripts(diff),
        accountData(diff),
        sponsorships(diff),
        scriptResults(diff),
        ethereumTransactionMeta(diff),
        diff.scriptsComplexity
      )
    )

  private def balances(diff: Diff, blockchain: Blockchain): Seq[S.Balance] =
    diff.portfolios.flatMap { case (address, Portfolio(wavesAmount, _, assets)) =>
      val assetBalances = assets.map { case (assetId, balance) =>
        val newBalance = blockchain.balance(address, assetId) + balance
        S.Balance(address.toByteString, Some(Amount(assetId.id.toByteString, newBalance)))
      }
      val newBalance   = blockchain.balance(address) + wavesAmount
      val wavesBalance = S.Balance(address.toByteString, Some(Amount(ByteString.EMPTY, newBalance)))
      assetBalances.toSeq :+ wavesBalance
    }.toSeq

  private def leaseBalances(diff: Diff, blockchain: Blockchain): Seq[S.LeaseBalance] =
    diff.portfolios.flatMap { case (address, Portfolio(_, lease, _)) =>
      if (lease != LeaseBalance.empty) {
        val bLease = blockchain.leaseBalance(address)
        Seq(S.LeaseBalance(address.toByteString, bLease.in + lease.in, bLease.out + lease.out))
      } else
        Nil
    }.toSeq

  private def assetStatics(diff: Diff): Seq[S.AssetStatic] =
    diff.issuedAssets.map { case (asset, info) =>
      S.AssetStatic(
        asset.id.toByteString,
        info.static.source.toByteString,
        info.static.issuer.toByteString,
        info.static.decimals,
        info.static.nft
      )
    }.toSeq

  private def assetVolumes(diff: Diff, blockchain: Blockchain): Seq[S.AssetVolume] = {
    val issued = diff.issuedAssets.view.mapValues(_.volume).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Right(volume))   => (asset, volume)
      case (asset, Ior.Both(_, volume)) => (asset, volume)
    }
    (issued |+| updated).map { case (asset, volume) =>
      val blockchainAsset = blockchain.assetDescription(asset)
      val newIsReissuable = blockchainAsset.map(_.reissuable && volume.isReissuable).getOrElse(volume.isReissuable)
      val newVolume       = blockchainAsset.map(_.totalVolume + volume.volume).getOrElse(volume.volume)
      val newVolumeBytes  = ByteString.copyFrom(newVolume.toByteArray)
      S.AssetVolume(asset.id.toByteString, newIsReissuable, newVolumeBytes)
    }.toSeq
  }

  private def assetNamesAndDescriptions(diff: Diff): Seq[S.AssetNameAndDescription] = {
    val issued = diff.issuedAssets.view.mapValues(_.dynamic).toMap
    val updated = diff.updatedAssets.collect {
      case (asset, Ior.Left(info))    => (asset, info)
      case (asset, Ior.Both(info, _)) => (asset, info)
    }
    (issued ++ updated).map { case (asset, info) =>
      S.AssetNameAndDescription(asset.id.toByteString, info.name.toStringUtf8, info.description.toStringUtf8, info.lastUpdatedAt)
    }.toSeq
  }

  private def assetScripts(diff: Diff): Seq[S.AssetScript] =
    diff.assetScripts.map { case (asset, script) =>
      S.AssetScript(asset.id.toByteString, script.fold(ByteString.EMPTY)(_.script.bytes().toByteString), script.fold(0L)(_.complexity))
    }.toSeq

  private def aliases(diff: Diff): Seq[S.Alias] =
    diff.aliases.map { case (alias, address) => S.Alias(address.toByteString, alias.name) }.toSeq

  private def orderFills(diff: Diff, blockchain: Blockchain): Seq[S.OrderFill] =
    diff.orderFills.map { case (orderId, value) =>
      val VolumeAndFee(newVolume, newFee) = value |+| blockchain.filledVolumeAndFee(orderId)
      S.OrderFill(orderId.toByteString, newVolume, newFee)
    }.toSeq

  private def leaseStates(diff: Diff): Seq[S.LeaseState] =
    diff.leaseState.map { case (leaseId, LeaseDetails(sender, recipient, amount, status, sourceId, height)) =>
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
        Some(PBRecipients.create(recipient)),
        sourceId.toByteString,
        height
      )
    }.toSeq

  private def accountScripts(diff: Diff): Seq[S.AccountScript] =
    diff.scripts.map { case (address, scriptOpt) =>
      scriptOpt.fold(
        S.AccountScript(
          address.toByteString,
          ByteString.EMPTY,
          ByteString.EMPTY,
          0,
          Map[String, Long]()
        )
      )(script =>
        S.AccountScript(
          address.toByteString,
          script.publicKey.toByteString,
          script.script.bytes().toByteString,
          script.verifierComplexity,
          script.complexitiesByEstimator.getOrElse(lastEstimator, Map())
        )
      )
    }.toSeq

  private def accountData(diff: Diff): Seq[S.AccountData] =
    diff.accountData.map { case (address, data) =>
      S.AccountData(address.toByteString, data.values.map(PBTransactions.toPBDataEntry).toSeq)
    }.toSeq

  private def sponsorships(diff: Diff): Seq[S.Sponsorship] =
    diff.sponsorship.collect { case (asset, SponsorshipValue(minFee)) =>
      S.Sponsorship(asset.id.toByteString, minFee)
    }.toSeq

  private def scriptResults(diff: Diff): Seq[S.ScriptResult] =
    diff.scriptResults.map { case (txId, script) =>
      S.ScriptResult(txId.toByteString, Some(InvokeScriptResult.toPB(script, addressForTransfer = true)))
    }.toSeq

  private def ethereumTransactionMeta(diff: Diff): Seq[S.EthereumTransactionMeta] =
    diff.ethereumTransactionMeta.map { case (txId, meta) =>
      val payload = meta.payload match {
        case Payload.Empty =>
          S.EthereumTransactionMeta.Payload.Empty
        case Payload.Invocation(value) =>
          S.EthereumTransactionMeta.Payload.Invocation(S.EthereumTransactionMeta.Invocation(value.functionCall, value.payments))
        case Payload.Transfer(value) =>
          S.EthereumTransactionMeta.Payload.Transfer(S.EthereumTransactionMeta.Transfer(value.publicKeyHash, value.amount))
      }
      S.EthereumTransactionMeta(txId.toByteString, payload)
    }.toSeq

  implicit val monoid: Monoid[StateSnapshot] = new Monoid[StateSnapshot] {
    override val empty = StateSnapshot(Vector(), TransactionStateSnapshot())
    override def combine(s1: StateSnapshot, s2: StateSnapshot): StateSnapshot = {

      // todo optimize
      def displaceData = {
        val updatedS2 = s1.current.accountData.map(d1 =>
          s2.current.accountData
            .find(d2 => d1.address == d2.address)
            .fold(d1)(d2 => d2.copy(entry = d1.entry.filterNot(e => d2.entry.exists(_.key == e.key)) ++ d2.entry))
        )
        updatedS2 ++ s2.current.accountData.filterNot(e => updatedS2.exists(_.address == e.address))
      }

      def displaceBy[A, B](f: TransactionStateSnapshot => Seq[A], k: A => B): Seq[A] = {
        val values1 = f(s1.current)
        val values2 = f(s2.current)
        values1.filterNot(v1 => values2.exists(v2 => k(v2) == k(v1))) ++ values2
      }

      StateSnapshot(
        s1.transactions ++ s2.transactions,
        TransactionStateSnapshot(
          displaceBy[S.Balance, (ByteString, ByteString)](_.balances, b => (b.address, b.getAmount.assetId)),
          displaceBy[S.LeaseBalance, ByteString](_.leaseBalances, _.address),
          displaceBy[S.AssetStatic, ByteString](_.assetStatics, _.assetId),
          displaceBy[S.AssetVolume, ByteString](_.assetVolumes, _.assetId),
          displaceBy[S.AssetNameAndDescription, ByteString](_.assetNamesAndDescriptions, _.assetId),
          displaceBy[S.AssetScript, ByteString](_.assetScripts, _.assetId),
          displaceBy[S.Alias, ByteString](_.aliases, _.address),
          displaceBy[S.OrderFill, ByteString](_.orderFills, _.orderId),
          displaceBy[S.LeaseState, ByteString](_.leaseStates, _.leaseId),
          displaceBy[S.AccountScript, ByteString](_.accountScripts, _.senderAddress),
          displaceData,
          displaceBy[S.Sponsorship, ByteString](_.sponsorships, _.assetId),
          displaceBy[S.ScriptResult, ByteString](_.scriptResults, _.transactionId),
          displaceBy[S.EthereumTransactionMeta, ByteString](_.ethereumTransactionMeta, _.transactionId),
          s1.scriptsComplexity + s2.scriptsComplexity
        )
      )
    }
  }
}
