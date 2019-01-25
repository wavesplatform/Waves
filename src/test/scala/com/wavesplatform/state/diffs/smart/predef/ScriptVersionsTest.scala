package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Testing
import com.wavesplatform.lang.Version._
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, TRUE}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.{EmptyBlockchain, compilerContext}
import fastparse.core.Parsed.Success
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.Gen
import shapeless.Coproduct

class ScriptVersionsTest extends FreeSpec with PropertyChecks with Matchers with TransactionGen {
  def eval[T <: EVALUATED](script: String,
                           version: Version,
                           tx: Transaction = null,
                           blockchain: Blockchain = EmptyBlockchain): Either[String, EVALUATED] = {
    val Success(expr, _) = Parser.parseScript(script)
    for {
      compileResult <- ExpressionCompilerV1(compilerContext(version, isAssetScript = false), expr)
      (typedExpr, _) = compileResult
      s <- ExprScript(version, typedExpr, checkSize = false)
      r <- ScriptRunner(blockchain.height, Coproduct(tx), blockchain, s, isTokenScript = false)._2
    } yield r

  }

  val duplicateNames =
    """
      |match tx {
      |  case tx: TransferTransaction => true
      |  case _ => false
      |}
    """.stripMargin

  val orderTypeBindings = "let t = Buy; t == Buy"

  "ScriptV1 allows duplicate names" in {
    forAll(transferV2Gen.flatMap(tx => Gen.oneOf(ExprV1, ExprV2).map(v => (tx, v)))) {
      case (tx, v) =>
        eval[EVALUATED](duplicateNames, v, tx) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV1 - does not have bindings defined in V2" in {
    eval[EVALUATED](orderTypeBindings, ExprV1) should produce("definition of 'Buy' is not found")
  }

  "ScriptV2" - {
    "allows duplicate names" in {
      forAll(transferV2Gen) { tx =>
        eval[EVALUATED](duplicateNames, ExprV2, tx) shouldBe Testing.evaluated(true)
      }
    }

    "has bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, ExprV2) shouldBe Testing.evaluated(true)
    }

    "only works after SmartAccountTrading feature activation" in {
      import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}

      val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccountTrading.id -> 3))
      val setup = for {
        master <- accountGen
        ts     <- positiveLongGen
        genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
        script  = ExprScript(ExprV2, TRUE, checkSize = false).explicitGet()
        tx      = SetScriptTransaction.selfSigned(1, master, Some(script), 100000, ts + 1).explicitGet()
      } yield (genesis, tx)

      forAll(setup) {
        case (genesis, tx) =>
          assertDiffEi(Seq(block(Seq(genesis))), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi should produce("Script version 2 has not been activated yet")
          }

          assertDiffEi(Seq(block(Seq(genesis)), block(Seq())), block(Seq(tx)), settings) { blockDiffEi =>
            blockDiffEi shouldBe 'right
          }
      }
    }
  }
}
