package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.SetScriptTransaction

class LazyFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransaction)] = {
    val untyped = Parser(code).get.value
    val typed   = TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
    preconditionsTransferAndLease(typed)
  }

  private val goodScript =
    """
      |
      | if (tx.type == 4)
      |   then isDefined(tx.assetId)==false
      |   else false
      |
      """.stripMargin

  private val badScript =
    """
      |
      | isDefined(tx.assetId) == false
      |
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(goodScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
  property("accessing field of transaction with check") {
    forAll(preconditionsTransferAndLease(badScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("doesn't contain asset id"))
    }
  }
}
