package com.wavesplatform.state.diffs.smart.scenarios

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.Version.ExprV1
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.diffs.smart._
import com.wavesplatform.utils.compilerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

    val scriptText =
      s"""
         |
         | match tx {
         |  case ttx: TransferTransaction | MassTransferTransaction =>
         |     sigVerify(ttx.bodyBytes,ttx.proofs[0],ttx.senderPublicKey)
         |  case other =>
         |     false
         | }
      """.stripMargin
    val untyped         = Parser.parseScript(scriptText).get.value
    val transferAllowed = ExpressionCompilerV1(compilerContext(ExprV1, isAssetScript = false), untyped).explicitGet()._1

    forAll(preconditionsTransferAndLease(transferAllowed)) {
      case (genesis, script, lease, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
