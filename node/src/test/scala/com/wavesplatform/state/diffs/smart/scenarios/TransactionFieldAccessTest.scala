package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.LeaseTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(compilerContext(V1, Expression, isAssetScript = false), untyped).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      |   case other =>
      |       false
      | }
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(script)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}
