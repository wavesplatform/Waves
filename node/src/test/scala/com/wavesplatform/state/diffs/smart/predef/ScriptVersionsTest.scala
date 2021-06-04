package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.Testing
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.state.diffs._
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, BooleanDataEntry, EmptyDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.{ScriptCompiler, ScriptRunner}
import com.wavesplatform.utils.EmptyBlockchain
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import shapeless.Coproduct

class ScriptVersionsTest extends FreeSpec with PropertyChecks with Matchers with TransactionGen {
  private def eval[T <: EVALUATED](
      script: String,
      version: StdLibVersion,
      tx: Transaction = transferV2Gen.sample.get,
      blockchain: Blockchain = EmptyBlockchain
  ): Either[String, EVALUATED] = {
    val expr = Parser.parseExpr(script).get.value
    for {
      compileResult <- ExpressionCompiler(compilerContext(version, Expression, isAssetScript = false), expr)
      (typedExpr, _) = compileResult
      s <- ExprScript(version, typedExpr, checkSize = false)
      r <- eval(s, tx, blockchain)
    } yield r
  }

  private def eval(
      script: Script,
      tx: Transaction,
      blockchain: Blockchain
  ): Either[String, EVALUATED] =
    ScriptRunner(Coproduct(tx), blockchain, script, isAssetScript = false, null)._3

  private val duplicateNames =
    """
      |match tx {
      |  case _: TransferTransaction => true
      |  case _ => false
      |}
    """.stripMargin

  private val orderTypeBindings = "let t = Buy; t == Buy"

  private val txById =
    """
      | let t = transactionById(base64'')
      | !isDefined(t)
      |
    """.stripMargin

  private val transferTxById =
    """
      | let t = transferTransactionById(base64'')
      | !isDefined(t)
      |
    """.stripMargin

  "ScriptV1 allows duplicate names" in {
    forAll(transferV2Gen.flatMap(tx => Gen.oneOf(V1, V2).map(v => (tx, v)))) {
      case (tx, v) =>
        eval[EVALUATED](duplicateNames, v, tx) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV1" - {
    "does not have bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, V1) should produce("definition of 'Buy' is not found")
    }

    "allow transactionById" in {
      eval[EVALUATED](txById, V2) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV2" - {
    "allows duplicate names" in {
      forAll(transferV2Gen) { tx =>
        eval[EVALUATED](duplicateNames, V2, tx) shouldBe Testing.evaluated(true)
      }
    }

    "has bindings defined in V2" in {
      eval[EVALUATED](orderTypeBindings, V2) shouldBe Testing.evaluated(true)
    }

    "allow transactionById" in {
      eval[EVALUATED](txById, V2) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV3" - {
    "disallow transactionById" in {
      eval[EVALUATED](txById, V3) should produce("Can't find a function 'transactionById'")
    }

    "add transferTransactionById" in {
      eval[EVALUATED](transferTxById, V3) shouldBe Testing.evaluated(true)
    }
  }

  "ScriptV4" - {
    "DataTransaction entry mapping" in {
      def compile(scriptText: String) =
        ScriptCompiler.compile(scriptText, ScriptEstimatorV3).explicitGet()._1

      def script(dApp: Boolean, version: StdLibVersion): Script =
        compile(
          s"""
             | {-# STDLIB_VERSION ${version.id}                        #-}
             | {-# SCRIPT_TYPE    ACCOUNT                              #-}
             | {-# CONTENT_TYPE ${if (dApp) "DAPP" else "EXPRESSION"}  #-}
             |
             | ${if (dApp) "@Verifier(tx) \n func verify() = " else ""}
             | match tx {
             |   case d:DataTransaction =>
             |       d.data.size() == 1 &&
             |       match d.data[0] {
             |         case entry: StringEntry =>
             |           entry.key == "key" &&
             |           entry.value == "value"
             |         case entry: IntegerEntry =>
             |           entry.key == "key" &&
             |           entry.value == 1
             |         case entry: BinaryEntry =>
             |           entry.key == "key" &&
             |           entry.value == base58'aaaa'
             |         case entry: BooleanEntry =>
             |           entry.key == "key" &&
             |           entry.value == true
             |         case entry: DeleteEntry =>
             |           entry.key == "key"
             |       }
             |   case _ =>
             |     sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
             | }
       """.stripMargin
        )

      val fixedBlockchain = new EmptyBlockchain {
        override def activatedFeatures: Map[Short, Int] = Map(BlockchainFeatures.SynchronousCalls.id -> 0)
      }

      for {
        isDApp      <- List(true, false)
        version     <- DirectiveDictionary[StdLibVersion].all.filter(if (isDApp) _ >= V3 else _ => true)
        activateFix <- List(true, false)
        entry <- List(
          StringDataEntry("key", "value"),
          IntegerDataEntry("key", 1),
          BinaryDataEntry("key", ByteStr.decodeBase58("aaaa").get),
          BooleanDataEntry("key", true),
          EmptyDataEntry("key")
        )
      } {
        val tx         = dataTransactionGen(1, withDeleteEntry = true).sample.get.copy(data = Seq(entry))
        val blockchain = if (activateFix) fixedBlockchain else EmptyBlockchain
        if (version >= V4) {
          if (!activateFix && isDApp && !entry.isInstanceOf[EmptyDataEntry])
            eval(script(isDApp, version), tx, blockchain) should produce("Match error")
          else
            eval(script(isDApp, version), tx, blockchain) shouldBe Testing.evaluated(true)
        } else
          (the[RuntimeException] thrownBy script(isDApp, version)).getMessage should include("Undefined type")
      }
    }
  }
}
