package scorex.transaction

import java.io._

import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{BeforeAndAfterAll, Matchers, PropSpec}
import scorex.transaction.state.database.state._

class RowSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with TransactionGen
  with BeforeAndAfterAll {

  val FileName = "object.data"

  override def afterAll(): Unit = {
    val testDataFile = new File(FileName)
    if (testDataFile.exists) testDataFile.delete
  }

  property("Row serialize and deserialize") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Long], Gen.posNum[Int]) { (payment: PaymentTransaction,
                                                                                     balance: Long,
                                                                                     fee: Long,
                                                                                     lastRowHeight: Int) =>

      val txs = List(FeesStateChange(fee), payment)
      TypedTransaction.parseBytes(payment.bytes).get shouldBe payment
      val row = Row(AccState(balance), txs, lastRowHeight)

      val objectOutputStream = new ObjectOutputStream(new FileOutputStream(FileName))
      objectOutputStream.writeObject(row)
      objectOutputStream.close()

      val objectInputStream = new ObjectInputStream(new FileInputStream(FileName))
      val des = objectInputStream.readObject().asInstanceOf[Row]
      des shouldBe row
    }
  }

}