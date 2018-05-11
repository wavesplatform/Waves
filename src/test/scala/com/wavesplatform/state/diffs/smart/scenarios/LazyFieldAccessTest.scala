package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.state._
import com.wavesplatform.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.GenesisTransaction
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.transfer._

class LazyFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV1)] = {
    val untyped = Parser(code).get.value
    assert(untyped.size == 1)
    val typed = CompilerV1(dummyTypeCheckerContext, untyped.head).explicitGet()
    preconditionsTransferAndLease(typed)
  }

  private val goodScript =
    """
      |
      | if (tx.type == 4)
      |   then isDefined(tx.transferAssetId)==false
      |   else false
      |
      """.stripMargin

  private val badScript =
    """
      |
      | isDefined(tx.transferAssetId) == false
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
          totalDiffEi should produce("doesn't transfer any asset"))
    }
  }
}
