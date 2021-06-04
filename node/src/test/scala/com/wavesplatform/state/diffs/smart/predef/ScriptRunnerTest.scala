package com.wavesplatform.state.diffs.smart.predef
import com.wavesplatform.NoShrink
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.FPPoSSelectorTest.accountGen
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.transfer.TransferTransaction
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import shapeless.Coproduct

class ScriptRunnerTest extends PropSpec with ScalaCheckPropertyChecks with Matchers with MockFactory with NoShrink with EitherValues {
  property("ScriptRunner.applyGeneric() avoids Blockchain calls") {
    val tx = TransferTransaction.selfSigned(1.toByte, accountGen.sample.get, accountGen.sample.get.toAddress, Waves, 1, Waves, 1, ByteStr.empty, 0)
    ScriptRunner.applyGeneric(
      Coproduct(tx.explicitGet()),
      stub[Blockchain],
      TestCompiler(V5).compileExpression("true"),
      isAssetScript = false,
      Coproduct(Recipient.Address(ByteStr.empty)),
      complexityLimit = 2000,
      default = null,
      useCorrectScriptVersion = true,
      fixUnicodeFunctions = true
    ) shouldBe (Nil, 0, Right(CONST_BOOLEAN(true)))
  }
}
