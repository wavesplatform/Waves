package scorex.transaction

import java.io.File

import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state._

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with PrivateMethodTester with OptionValues with TransactionGen {

  val folder = "/tmp/scorex/test"
  new File(folder).mkdirs()
  val stateFile = folder + "state.dat"

  val state = new StoredState(Some(stateFile))
  val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
  val applyMethod = PrivateMethod[Unit]('applyChanges)

  property("private methods") {

    forAll(paymentGenerator, Gen.posNum[Long]) { (tx: PaymentTransaction,
                                                  balance: Long) =>
      state.balance(testAdd) shouldBe 0
      state invokePrivate applyMethod(Map(testAdd ->(AccState(balance), Seq(FeesStateChange(balance), tx, tx))))
      state.balance(testAdd) shouldBe balance
      state.included(tx).value shouldBe state.stateHeight
      state invokePrivate applyMethod(Map(testAdd ->(AccState(0L), Seq(tx))))
    }
  }

  property("Reopen state") {
    val balance = 1234L
    state invokePrivate applyMethod(Map(testAdd ->(AccState(balance), Seq(FeesStateChange(balance)))))
    state.balance(testAdd) shouldBe balance
    state.finalize()

    val state2 = new StoredState(Some(stateFile))
    state2.balance(testAdd) shouldBe balance
    state2 invokePrivate applyMethod(Map(testAdd ->(AccState(0L), Seq())))
  }

}
