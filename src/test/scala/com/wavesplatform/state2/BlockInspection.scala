package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.state2.reader.StateReaderImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Account, AddressScheme}
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.transaction.ValidationError.TransactionParameterValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.StoredBlockchain

import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable


object BlockInspection extends App {

  import StateResponseComparisonTests._

  class InspectionStateReader(val p: JavaMapStorage) extends StateReaderImpl(p) {
    lazy val allTxs: List[(Int, Transaction)] = p.transactions.asScala
      .map { case (id, (height, txsBytes)) => (height, TransactionParser.parseBytes(txsBytes).get) }
      .toList

    def findTransactionsWithAsset(assetId: String): List[(Int, Transaction)] = allTxs.collect {
      case (h, itx: IssueTransaction) if Base58.encode(itx.id) == assetId => (h, itx)
      case (h, itx: ReissueTransaction) if Base58.encode(itx.assetId) == assetId => (h, itx)
      case (h, itx: BurnTransaction) if Base58.encode(itx.assetId) == assetId => (h, itx)
      case (h, itx: TransferTransaction) if itx.assetId.map(Base58.encode).contains(assetId) => (h, itx)
      case (h, itx: TransferTransaction) if itx.feeAssetId.map(Base58.encode).contains(assetId) => (h, itx)
    }
  }


  def a1() {
    val maps = new MVStorePrimitiveImpl(new MVStore.Builder().fileName("C:\\Users\\ilyas\\Desktop\\new.store").open())
    val inspection = new InspectionStateReader(maps)
    val txs = inspection.accountTransactionIds(Account.fromString("3P7GQo48n1SM7EZXnmNBRMcD5oDwKXf8SSm").right.get).map(inspection.transactionInfo(_)).toList
    println(txs)
  }

  def a2() {
    val emptyDiff: Diff = Monoid[Diff].empty
    val currentMainnetStore = BlockStorageImpl.createMVStore(BlocksOnDisk)
    val currentMainnet = storedBC(oldState(currentMainnetStore), new StoredBlockchain(currentMainnetStore))
    val map: IndexedSeq[Diff] = Range(1, currentMainnet.history.height() + 1)
      .map(h => currentMainnet.history.blockAt(h).get)
      .filter(b => b.signerData.generator.address == "3P7GQo48n1SM7EZXnmNBRMcD5oDwKXf8SSm")
      .map(_.feesDistribution.map { case (AssetAcc(account, maybeAssetId), feeVolume) =>
        account -> (maybeAssetId match {
          case None => Portfolio(feeVolume, feeVolume, Map.empty)
          case Some(assetId) => Portfolio(0L, 0L, Map(EqByteArray(assetId) -> feeVolume))
        })
      }.foldLeft(emptyDiff) { case (combinedDiff, (acc, portfolio)) =>
        combinedDiff.combine(new Diff(Map.empty, Map(acc -> portfolio), Map.empty, Map.empty))
      }
      )


    val res = Monoid[Diff].combineAll(map)
    println(res)
  }

  def a3() {
    import StateResponseComparisonTests._
    setNetworkByte()
    val maps = new MVStorePrimitiveImpl(new MVStore.Builder().fileName("C:\\Users\\ilyas\\Desktop\\new.store").open())
    val inspection = new InspectionStateReader(maps)
    println(inspection.p.portfolios.asScala.values.map(_._1).sum)
  }

  def a4(): Unit = {

  }
}
