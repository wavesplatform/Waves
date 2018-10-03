package com.wavesplatform.transaction

import com.wavesplatform.TransactionGen
import com.wavesplatform.account.Address
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.transaction.transfer._
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, Matchers, PropSpec}

class FeeCalculatorSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen with MockFactory {

  implicit class ConditionalAssert(v: Either[_, _]) {

    def shouldBeRightIf(cond: Boolean): Assertion = {
      if (cond) {
        v shouldBe an[Right[_, _]]
      } else {
        v shouldBe an[Left[_, _]]
      }
    }
  }

  property("Transfer transaction ") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(transferV1Gen) { tx: TransferTransactionV1 =>
      if (tx.feeAssetId.isEmpty) {
        feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
      } else {
        feeCalc.enoughFee(tx) shouldBe an[Right[_, _]]
      }
    }
  }

  property("Payment transaction ") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(paymentGen) { tx: PaymentTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000)
    }
  }

  property("Issue transaction ") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(issueGen) { tx: IssueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 100000000)
    }
  }

  property("Reissue transaction ") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(reissueGen) { tx: ReissueTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 200000)
    }
  }

  property("Burn transaction ") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(burnGen) { tx: BurnTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 300000)
    }
  }

  property("Lease transaction") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(leaseGen) { tx: LeaseTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 400000)
    }
  }

  property("Lease cancel transaction") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(leaseCancelGen) { tx: LeaseCancelTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 500000)
    }
  }

  property("Create alias transaction") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(createAliasGen) { tx: CreateAliasTransaction =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= 600000)
    }
  }

  property("Data transaction") {
    val feeCalc = new FeeCalculator(noScriptBlockchain)
    forAll(dataTransactionGen) { tx =>
      feeCalc.enoughFee(tx) shouldBeRightIf (tx.fee >= Math.ceil(tx.bytes().length / 1024.0) * 100000)
    }
  }

  private def createBlockchain(accountScript: Address => Option[Script]): Blockchain = {
    val r = stub[Blockchain]
    (r.accountScript _).when(*).onCall((addr: Address) => accountScript(addr)).anyNumberOfTimes()
    r
  }

  private def noScriptBlockchain: Blockchain = createBlockchain(_ => None)
}
