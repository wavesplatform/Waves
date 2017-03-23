package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.state2.reader.StateReaderImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.{Account, AddressScheme}
import scorex.crypto.encode.Base58
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction, TransactionParser}
import scorex.transaction.ValidationError.TransactionParameterValidationError
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.StoredBlockchain

import scala.collection.JavaConverters.mapAsScalaMapConverter
import scala.collection.mutable


object BlockInspection extends App {

  class InspectionStateReader(val p: JavaMapStorage) extends StateReaderImpl(p) {
    lazy val allTxs: List[(Int, Transaction)] = p.transactions.asScala
      .map { case (id, (height, txsBytes)) => (height, TransactionParser.parseBytes(txsBytes).get) }
      .toList

    def findTxsOfAccount(address: String): List[(Int, Transaction)] = allTxs.collect {
      case (h, tx: GenesisTransaction) if tx.recipient.address == address => (h, tx)
      case (h, tx: PaymentTransaction) if tx.recipient.address == address => (h, tx)
      case (h, tx: TransferTransaction) if tx.recipient.asInstanceOf[Account].address == address => (h, tx)
      case (h, tx: Transaction) if tx.artifSender().address == address => (h, tx)
    }

    def findTransactionsWithAsset(assetId: String): List[(Int, Transaction)] = allTxs.collect {
      case (h, itx: IssueTransaction) if Base58.encode(itx.id) == assetId => (h, itx)
      case (h, itx: ReissueTransaction) if Base58.encode(itx.assetId) == assetId => (h, itx)
      case (h, itx: BurnTransaction) if Base58.encode(itx.assetId) == assetId => (h, itx)
      case (h, itx: TransferTransaction) if itx.assetId.map(Base58.encode).contains(assetId) => (h, itx)
      case (h, itx: TransferTransaction) if itx.feeAssetId.map(Base58.encode).contains(assetId) => (h, itx)
    }
  }

  import StateResponseComparisonTests._

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'W'
  }
  val maps = new MVStorePrimitiveImpl(new MVStore.Builder().fileName("C:\\Users\\ilyas\\Desktop\\new.store").open())
  val inspection = new InspectionStateReader(maps)
  val txs = inspection.findTxsOfAccount("3P7GQo48n1SM7EZXnmNBRMcD5oDwKXf8SSm")
  println(txs)
}
