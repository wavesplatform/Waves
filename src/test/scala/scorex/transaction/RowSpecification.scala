package scorex.transaction

import java.io.File

import org.h2.mvstore.{MVMap, MVStore, WriteBuffer}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import scorex.transaction.state.database.state._
import scorex.utils.LogMVMapBuilder

class RowSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with BeforeAndAfterAll {

  val Filename = "rows.dat"

  override def afterAll(): Unit = {
    val testDataFile = new File(Filename)
    if (testDataFile.exists) testDataFile.delete
  }

  property("Row serialize and deserialize") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Long], Gen.posNum[Int]) { (payment: PaymentTransaction,
                                                                                     balance: Long,
                                                                                     fee: Long,
                                                                                     lastRowHeight: Int) =>

      val txs = List(FeesStateChange(fee), payment)
      TypedTransaction.parseBytes(payment.bytes).get shouldBe payment

      val row = Row(AccState(balance), txs.map(_.id), lastRowHeight)
      val restored = Row.deserialize(row.bytes)

      restored shouldBe row
    }
  }

  property("RowDataType serialize and deserialize") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Long], Gen.posNum[Int]) { (payment: PaymentTransaction,
                                                                                     balance: Long,
                                                                                     fee: Long,
                                                                                     lastRowHeight: Int) =>

      val txs = List(FeesStateChange(fee), payment)
      TypedTransaction.parseBytes(payment.bytes).get shouldBe payment

      val row = Row(AccState(balance), txs.map(_.id), lastRowHeight)
      val buffer = new WriteBuffer(0)
      RowDataType.write(buffer, row)

      val buf = buffer.getBuffer
      buf.flip()
      val restored = RowDataType.read(buf)

      restored shouldBe row
    }
  }

  property("Row save restore should work") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Long], Gen.posNum[Int]) { (payment: PaymentTransaction,
                                                                                     balance: Long,
                                                                                     fee: Long,
                                                                                     lastRowHeight: Int) =>

      val db1 = new MVStore.Builder().fileName(Filename).compress().open()
      val table: MVMap[Int, Row] = db1.openMap("rows", new LogMVMapBuilder[Int, Row].valueType(RowDataType))

      val txs = List(FeesStateChange(fee), payment)
      val row = Row(AccState(balance), txs.map(_.id), lastRowHeight)

      table.put(lastRowHeight, row)
      closeDb(db1)

      val db2 = new MVStore.Builder().fileName(Filename).compress().open()
      val table2: MVMap[Int, Row] = db2.openMap("rows", new LogMVMapBuilder[Int, Row].valueType(RowDataType))

      val restored = table2.get(lastRowHeight)
      closeDb(db2)

      restored shouldBe row
    }

  }

  private def closeDb(db: MVStore): Unit = {
    db.close()
    while (!db.isClosed) Thread.sleep(1)
  }

}