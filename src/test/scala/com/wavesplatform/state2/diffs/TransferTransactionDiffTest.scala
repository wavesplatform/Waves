package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import cats.syntax.all._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2._
import org.h2.mvstore.MVStore
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.Account
import scorex.transaction.TransactionGen

class TransferTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  ignore("Block diff for transfer is zero") {
    // we need to build a block, than build a blockdiff to ensure that
    forAll(transferGenerator suchThat (_.recipient.isInstanceOf[Account])) { transfer =>
      val p = new MVStorePrimitiveImpl(new MVStore.Builder().open())
      val state = new StateWriterImpl(p)

      val account = transfer.sender.toAccount
      ensureSenderHasEnoughBalance(state)(account, Set(transfer.feeAssetId, transfer.assetId).flatten.toList.map(EqByteArray))

      val diffEi = TransferTransactionDiff.apply(state, ???, ???, 1)(transfer)
      val portfolioChanges: List[Portfolio] = diffEi.right.get.portfolios.values.toList
      val totalDiff = Monoid[Portfolio].combineAll(portfolioChanges)
      totalDiff.balance shouldBe 0
      totalDiff.effectiveBalance shouldBe 0
      totalDiff.assets.values.foreach(_ shouldBe 0)
    }
  }

}
