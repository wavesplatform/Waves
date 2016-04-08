package scorex.lagonaki.integration

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.AccState
import scorex.transaction.{FeesStateChange, PaymentTransaction, TransactionGen}

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
  with PrivateMethodTester with OptionValues with TransactionGen {

  val state = new StoredState(None)

  property("private methods") {
    forAll(paymentGenerator, Gen.posNum[Long], Gen.posNum[Int]) { (tx: PaymentTransaction,
                                                                   balance: Long,
                                                                   lastRowHeight: Int) =>
      val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
      val applyMethod = PrivateMethod[Unit]('applyChanges)
      state.balance(testAdd) shouldBe 0
      state invokePrivate applyMethod(Map(testAdd ->(AccState(balance), Seq(FeesStateChange(balance), tx))))
      state.balance(testAdd) shouldBe balance
      state.included(tx).value shouldBe state.stateHeight
      state invokePrivate applyMethod(Map(testAdd ->(AccState(0L), Seq(tx))))
    }
  }

}
