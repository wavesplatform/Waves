package com.wavesplatform.state

import cats.data.Ior
import cats.implicits.{catsSyntaxEitherId, catsSyntaxSemigroup, toTraverseOps}
import com.google.common.hash.{BloomFilter, Funnels}
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.protobuf.EthereumTransactionMeta
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction

import scala.collection.immutable.VectorMap
import scala.util.chaining.*

case class Diff(
    transactions: Vector[NewTransactionInfo],
    portfolios: Map[Address, Portfolio],
    issuedAssets: VectorMap[IssuedAsset, NewAssetInfo],
    updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]],
    aliases: Map[Alias, Address],
    orderFills: Map[ByteStr, VolumeAndFee],
    leaseState: Map[ByteStr, LeaseDetails],
    scripts: Map[PublicKey, Option[AccountScriptInfo]],
    assetScripts: Map[IssuedAsset, Option[AssetScriptInfo]],
    accountData: Map[Address, Map[String, DataEntry[?]]],
    sponsorship: Map[IssuedAsset, Sponsorship],
    scriptsRun: Int,
    scriptsComplexity: Long,
    scriptResults: Map[ByteStr, InvokeScriptResult],
    ethereumTransactionMeta: Map[ByteStr, EthereumTransactionMeta],
    transactionFilter: Option[BloomFilter[Array[Byte]]]
) {
  def containsTransaction(txId: ByteStr): Boolean =
    transactions.nonEmpty && transactionFilter.exists(_.mightContain(txId.arr)) && transactions.exists(_.transaction.id() == txId)

  def transaction(txId: ByteStr): Option[NewTransactionInfo] =
    if (transactions.nonEmpty && transactionFilter.exists(_.mightContain(txId.arr)))
      transactions.find(_.transaction.id() == txId)
    else None

  def combineF(newer: Diff): Either[String, Diff] =
    for {
      portfolios <- Portfolio.combine(portfolios, newer.portfolios)
      orderFills <- {
        val combinedOrders =
          orderFills.toSeq
            .traverse { case kv @ (orderId, value) =>
              newer.orderFills.get(orderId).fold(kv.asRight[String])(value.combineE(_).map(orderId -> _))
            }
            .map(_.toMap)
        combinedOrders.map(co => co ++ newer.orderFills.filterNot { case (id, _) => co.contains(id) })
      }
      newTransactions = if (transactions.isEmpty) newer.transactions else transactions ++ newer.transactions
      newFilter = transactionFilter match {
        case Some(bf) =>
          newer.transactions.foreach(nti => bf.put(nti.transaction.id().arr))
          Some(bf)
        case None =>
          newer.transactionFilter
      }
    } yield Diff(
      transactions = newTransactions,
      portfolios = portfolios,
      issuedAssets = issuedAssets ++ newer.issuedAssets,
      updatedAssets = updatedAssets |+| newer.updatedAssets,
      aliases = aliases ++ newer.aliases,
      orderFills = orderFills,
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

object Diff {
  def apply(
      portfolios: Map[Address, Portfolio] = Map.empty,
      issuedAssets: VectorMap[IssuedAsset, NewAssetInfo] = VectorMap.empty,
      updatedAssets: Map[IssuedAsset, Ior[AssetInfo, AssetVolumeInfo]] = Map.empty,
      aliases: Map[Alias, Address] = Map.empty,
      orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
      leaseState: Map[ByteStr, LeaseDetails] = Map.empty,
      scripts: Map[PublicKey, Option[AccountScriptInfo]] = Map.empty,
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
      scripts: Map[PublicKey, Option[AccountScriptInfo]] = Map.empty,
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

  private def combine[K, IK, IV](first: Map[K, Map[IK, IV]], second: Map[K, Map[IK, IV]]): Map[K, Map[IK, IV]] = {
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
  }
}
