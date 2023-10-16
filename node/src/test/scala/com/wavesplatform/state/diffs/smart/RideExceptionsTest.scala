package com.wavesplatform.state.diffs.smart

import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.LightNode
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.directives.values.V7
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.evaluator.FunctionIds.{ACCOUNTWAVESBALANCE, CALLDAPP}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.test.DomainPresets.{TransactionStateSnapshot, WavesSettingsOps}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.TxHelpers.*

class RideExceptionsTest extends PropSpec with WithDomain {
  property("throwing java exception from ride functions should correctly fail or reject invoke after light node activation") {
    assert(
      FUNCTION_CALL(Native(ACCOUNTWAVESBALANCE), List(REF("unit"))),
      "Unexpected recipient type Unit",
      rejectBefore = false
    )
    assert(
      FUNCTION_CALL(Native(CALLDAPP), List(CONST_LONG(1), CONST_LONG(1), CONST_LONG(1), CONST_LONG(1))),
      "Unexpected recipient arg",
      rejectBefore = true
    )
    assert(
      FUNCTION_CALL(Native(CALLDAPP), List(FUNCTION_CALL(User("Address"), List(CONST_LONG(1))), CONST_LONG(1), CONST_LONG(1), CONST_LONG(1))),
      "Unexpected address bytes",
      rejectBefore = true
    )
    assert(
      FUNCTION_CALL(
        Native(CALLDAPP),
        List(FUNCTION_CALL(User("Alias"), List(CONST_STRING("alias").explicitGet())), CONST_LONG(1), CONST_LONG(1), CONST_LONG(1))
      ),
      "Alias 'alias:T:alias' does not exist",
      rejectBefore = true
    )
    assert(
      FUNCTION_CALL(
        Native(CALLDAPP),
        List(FUNCTION_CALL(User("Alias"), List(CONST_LONG(1))), CONST_LONG(1), CONST_LONG(1), CONST_LONG(1))
      ),
      "Unexpected alias arg",
      rejectBefore = true
    )
    assert(
      FUNCTION_CALL(
        Native(CALLDAPP),
        List(FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())), CONST_LONG(1), CONST_LONG(1), CONST_LONG(1))
      ),
      "Unexpected name arg",
      rejectBefore = true
    )
    assert(
      FUNCTION_CALL(
        Native(CALLDAPP),
        List(
          FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(ByteStr.empty).explicitGet())),
          REF("unit"),
          CONST_LONG(1),
          ARR(Vector(CONST_LONG(1)), limited = false).explicitGet()
        )
      ),
      "Unexpected payment arg",
      rejectBefore = true
    )
  }

  private def assert(expr: EXPR, error: String, rejectBefore: Boolean): Block =
    withDomain(TransactionStateSnapshot.setFeaturesHeight(LightNode -> 6), AddrWithBalance.enoughBalances(defaultSigner, secondSigner)) { d =>
      val func = FUNC("default", Nil, expr)
      val dApp = DApp(DAppMeta(), Nil, List(CallableFunction(CallableAnnotation("i"), func)), None)
      d.appendBlock(setScript(secondSigner, ContractScriptImpl(V7, dApp)))

      // before activation
      if (rejectBefore)
        d.appendBlockE(invoke()) should produce(error)
      else
        d.appendAndAssertFailed(invoke(), error)

      // before activation with enough complexity to fail
      val complexCond = TestCompiler(V7).compileExpression(s"${(1 to 6).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}")
      val complexExpr = IF(complexCond.expr, TRUE, expr)
      val complexFunc = FUNC("default", Nil, complexExpr)
      val complexDApp = DApp(DAppMeta(), Nil, List(CallableFunction(CallableAnnotation("i"), complexFunc)), None)
      d.appendBlock(setScript(secondSigner, ContractScriptImpl(V7, complexDApp)))
      if (rejectBefore)
        d.appendBlockE(invoke()) should produce(error)
      else
        d.appendAndAssertFailed(invoke(), error)

      // after activation
      d.blockchain.isFeatureActivated(LightNode) shouldBe false
      d.appendBlock(setScript(secondSigner, ContractScriptImpl(V7, dApp)))
      if (rejectBefore) {
        d.appendBlock()
        d.appendBlock()
      }
      d.blockchain.isFeatureActivated(LightNode) shouldBe true
      d.appendBlockE(invoke()) should produce(error)

      // after activation with enough complexity to fail
      d.appendBlock(setScript(secondSigner, ContractScriptImpl(V7, complexDApp)))
      d.appendAndAssertFailed(invoke(), error)
    }
}
