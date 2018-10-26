package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.TransactionGen
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.EitherExt2
import com.wavesplatform.state.diffs._
import com.wavesplatform.lang.ScriptVersion
import com.wavesplatform.lang.ScriptVersion.Versions._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.v1.ScriptV1
import com.wavesplatform.utils.{EmptyBlockchain, compilerContext}
import fastparse.core.Parsed.Success
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import shapeless.Coproduct

class ScriptVersionsTest extends FreeSpec with PropertyChecks with Matchers with TransactionGen {
  def eval[T](script: String, version: ScriptVersion, tx: Transaction = null, blockchain: Blockchain = EmptyBlockchain): Either[String, T] = {
    val Success(expr, _) = Parser(script)
    for {
      compileResult <- CompilerV1(compilerContext(version), expr)
      (typedExpr, _) = compileResult
      s <- ScriptV1(version, typedExpr, checkSize = false)
      r <- ScriptRunner[T](blockchain.height, Coproduct(tx), blockchain, s)._2
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

  "ScriptV1" - {
    "forbids duplicate names" in {
      forAll(transferV1Gen) { tx =>
        eval[Boolean](duplicateNames, V1, tx) should produce("duplicate variable names")
      }
    }

    "does not have bindings defined in V2" in {
      eval[Boolean](orderTypeBindings, V1) should produce("definition of 'Buy' is not found")
    }
  }

  "ScriptV2" - {
    "allows duplicate names" in {
      forAll(transferV2Gen) { tx =>
        eval[Boolean](duplicateNames, V2, tx) shouldBe Right(true)
      }
    }

    "has bindings defined in V2" in {
      eval[Boolean](orderTypeBindings, V2) shouldBe Right(true)
    }

    "only works after SmartAccountTrading feature activation" in {
      import com.wavesplatform.lagonaki.mocks.TestBlock.{create => block}

      val settings = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccountTrading.id -> 3))
      val setup = for {
        master <- accountGen
        ts     <- positiveLongGen
        genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
        script  = ScriptV1(V2, TRUE, checkSize = false).explicitGet()
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
