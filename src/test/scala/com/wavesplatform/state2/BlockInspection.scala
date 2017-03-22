package com.wavesplatform.state2

import cats._
import cats.implicits._
import cats.Monoid
import com.wavesplatform.state2.reader.StateReaderImpl
import org.h2.mvstore.MVStore
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.AddressScheme
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser
import scorex.transaction.ValidationError.TransactionParameterValidationError
import scorex.transaction.assets.ReissueTransaction
import scorex.transaction.state.database.BlockStorageImpl
import scorex.transaction.state.database.blockchain.StoredBlockchain

import scala.collection.JavaConverters.mapAsScalaMapConverter


object BlockInspection extends App {

  class InspectionStateReader(val p: JavaMapStorage) extends StateReaderImpl(p) {

    val allTxs = p.transactions.asScala.map { case (id, (height, txsBytes)) => (height, TransactionParser.parseBytes(txsBytes).get)
    }
  }

  import StateResponseComparisonTests._

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = 'W'
  }
  val maps = new MVStorePrimitiveImpl(new MVStore.Builder().fileName("C:\\Users\\ilyas\\Desktop\\new.store").open())
  val inspection = new InspectionStateReader(maps)
  val r = inspection.allTxs
    .filter { case (h, tx) => tx.isInstanceOf[ReissueTransaction] }
    .map { case (h, tx) => (h, tx.asInstanceOf[ReissueTransaction]) }
  //      .filter { case (h, tx) => Base58.encode(tx.id) == "GQr2fpkfmWjMaZCbqMxefbiwgvpcNgYdev7xpuX6xqcE" }

  println(r)
}
