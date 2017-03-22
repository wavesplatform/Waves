package com.wavesplatform.state2

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.diffs.PaymentTransactionDiff
import org.h2.mvstore.MVStore
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.transaction.TransactionGen

class StateWriterImplTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("exposes 1 paymentTransaction of a sender when paying to self") {
    forAll(selfPaymentGenerator) { payment =>
      val p = new MVStorePrimitiveImpl(new MVStore.Builder().open())
      val state = new StateWriterImpl(p)

      val account = payment.sender.toAccount
      state.applyBlockDiff(Diff(Map.empty,
        Map(account -> Portfolio(payment.amount + payment.fee, payment.amount + payment.fee, Map.empty)), Map.empty).asBlockDiff)

      val diffEi = PaymentTransactionDiff.apply(state, FunctionalitySettings.MAINNET, 1)(payment)
      diffEi.right.get.accountTransactionIds(EqByteArray(account.bytes)).size shouldBe 1
    }
  }
}
