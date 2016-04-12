package scorex.lagonaki.integration

import org.h2.mvstore.{MVMap, MVStore}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state._
import scorex.transaction.{FeesStateChange, PaymentTransaction, TransactionGen}

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
  with PrivateMethodTester with OptionValues with TransactionGen {


  val state = new StoredState(None) // Don't work for Some(file)

  property("private methods") {
    val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
    val applyMethod = PrivateMethod[Unit]('applyChanges)

    forAll(paymentGenerator, Gen.posNum[Long]) { (tx: PaymentTransaction,
                                                  balance: Long) =>
      state.balance(testAdd) shouldBe 0
      state invokePrivate applyMethod(Map(testAdd ->(AccState(balance), Seq(FeesStateChange(balance), tx))))
      state.balance(testAdd) shouldBe balance
      state.included(tx).value shouldBe state.stateHeight
      state invokePrivate applyMethod(Map(testAdd ->(AccState(0L), Seq(tx))))
    }
  }

  property("DB reopen") {
    val db = new MVStore.Builder().fileName("/tmp/mvstoretest.dat").open()

    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Int]) { (tx: PaymentTransaction,
                                                                   balance: Long,
                                                                   lastRowHeight: Int) =>

      val map: MVMap[Int, Row] = db.openMap("map", new MVMap.Builder[Int, Row])
      val row = Row(AccState(balance), Seq(tx), lastRowHeight)
      map.put(lastRowHeight, row)
      map.get(lastRowHeight) shouldBe row
    }
    db.commit()

  }
}
