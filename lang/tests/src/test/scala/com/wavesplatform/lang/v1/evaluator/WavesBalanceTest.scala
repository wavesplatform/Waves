package com.wavesplatform.lang.v1.evaluator

import cats.Id
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.test.PropSpec

class WavesBalanceTest extends PropSpec {
  private val version = V6
  private val ctx     = lazyContexts((DirectiveSet(version, Account, DApp).explicitGet(), true, true, true))()

  // The generating balance walks up to 1000 blocks of balance history, so it is computed only when a script uses it.
  private def evaluate(script: String): (EXPR, Int) = {
    var computed = 0
    val env = Common.emptyBlockchainEnvironment(wavesBalance =
      _ =>
        Right(Environment.BalanceDetails(available = 1, regular = 2, generatingBalance = { computed += 1; computed }, effective = 4))
    )
    val expr = ExpressionCompiler(ctx.compilerContext, version, Parser.parseExpr(script).get.value).explicitGet()._1
    val result = EvaluatorV2
      .applyLimitedCoeval(expr, LogExtraInfo(), Int.MaxValue, ctx.evaluationContext(env), version, true, true, false, false, true)
      .value()
      .explicitGet()
      ._1
    (result, computed)
  }

  private val balance = "wavesBalance(Address(base58'3MuVqVJGmFsHeuFni5RbjRmALuGCkEwzZtC'))"

  property("wavesBalance does not compute the generating balance of an object whose generating field is not used") {
    evaluate(s"$balance.available + $balance.regular + $balance.effective") shouldBe (CONST_LONG(7), 0)
  }

  property("wavesBalance computes the generating balance when the generating field is read") {
    evaluate(s"$balance.generating") shouldBe (CONST_LONG(1), 1)
  }

  // Each computed generating balance differs here, so the objects are equal only if the comparison skipped them.
  property("comparing balance details compares their generating balances") {
    evaluate(s"$balance == $balance") shouldBe (CONST_BOOLEAN(false), 2)
  }
}
