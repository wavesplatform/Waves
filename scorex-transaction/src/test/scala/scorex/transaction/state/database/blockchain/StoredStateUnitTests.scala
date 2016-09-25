package scorex.transaction.state.database.blockchain

import java.io.File

import org.h2.mvstore.MVStore
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.account.Account
import scorex.transaction._
import scorex.transaction.assets.IssueTransaction
import scorex.transaction.state.database.state._

import scala.util.Random

class StoredStateUnitTests extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers
with PrivateMethodTester with OptionValues with TransactionGen {

  val folder = "/tmp/scorex/test/"
  new File(folder).mkdirs()
  val stateFile = folder + "state.dat"
  new File(stateFile).delete()

  val db = new MVStore.Builder().fileName(stateFile).compress().open()
  val state = new StoredState(db)
  val testAdd = "aPFwzRp5TXCzi6DSuHmpmbQunopXRuxLk"
  val testAcc = new Account(testAdd)

  val applyChanges = PrivateMethod[Unit]('applyChanges)
  val calcNewBalances = PrivateMethod[Unit]('calcNewBalances)

  property("Reissue asset") {
    forAll(issueReissueGenerator) { pair =>
      val issueTx: IssueTransaction = pair._1
      val reissueTx: IssueTransaction = pair._2
      val assetAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))

      state.applyChanges(state.calcNewBalances(Seq(issueTx), Map()))

      state.isValid(reissueTx, Int.MaxValue) shouldBe issueTx.reissuable
    }
  }

  property("Issue asset") {
    forAll(issueGenerator) { issueTx: IssueTransaction =>
      val assetAcc = AssetAcc(issueTx.sender, Some(issueTx.assetId))
      val networkAcc = AssetAcc(issueTx.sender, None)

      //set some balance
      val genes = GenesisTransaction(issueTx.sender, issueTx.fee + Random.nextInt, issueTx.timestamp - 1)
      state.applyChanges(state.calcNewBalances(Seq(genes), Map()))
      state.assetBalance(assetAcc) shouldBe 0
      state.assetBalance(networkAcc) shouldBe genes.amount
      state.balance(issueTx.sender) shouldBe genes.amount

      //issue asset
      state.assetBalance(assetAcc) shouldBe 0
      val newBalances = state.calcNewBalances(Seq(issueTx), Map())
      state.applyChanges(newBalances)
      state.assetBalance(assetAcc) shouldBe issueTx.quantity
      state.assetBalance(networkAcc) shouldBe (genes.amount - issueTx.fee)
    }
  }

  property("Applying transactions") {
    val testAssetAcc = AssetAcc(testAcc, None)
    forAll(paymentGenerator, Gen.posNum[Long]) { (tx: PaymentTransaction,
                                                  balance: Long) =>
      state.balance(testAcc) shouldBe 0
      state.assetBalance(testAssetAcc) shouldBe 0
      state invokePrivate applyChanges(Map(testAssetAcc ->(AccState(balance), Seq(FeesStateChange(balance), tx, tx))))
      state.balance(testAcc) shouldBe balance
      state.assetBalance(testAssetAcc) shouldBe balance
      state.included(tx).value shouldBe state.stateHeight
      state invokePrivate applyChanges(Map(testAssetAcc ->(AccState(0L), Seq(tx))))
    }
  }

  property("Reopen state") {
    val testAssetAcc = AssetAcc(testAcc, None)
    val balance = 1234L
    state invokePrivate applyChanges(Map(testAssetAcc ->(AccState(balance), Seq(FeesStateChange(balance)))))
    state.balance(testAcc) shouldBe balance
    db.close()

    val state2 = new StoredState(new MVStore.Builder().fileName(stateFile).compress().open())
    state2.balance(testAcc) shouldBe balance
    state2 invokePrivate applyChanges(Map(testAssetAcc ->(AccState(0L), Seq())))
  }

}
