package com.wavesplatform.transaction.smart.script
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ScriptType
import com.wavesplatform.lang.StdLibVersion.{StdLibVersion, V1}
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds.SIGVERIFY
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator}
import com.wavesplatform.transaction.smart.script.v1.ExprScript.checksumLength
import com.wavesplatform.utils.{functionCosts, varNames}
import monix.eval.Coeval

object ContractScript {

  private val maxComplexity = 20 * functionCosts(V1)(FunctionHeader.Native(SIGVERIFY))()

  def apply(version: StdLibVersion, contract: Contract): Either[String, Script] = {
    for {
      funcMaxComplexity <- estimateComplexity(version, contract)
      _ <- Either.cond(
        funcMaxComplexity._2 <= maxComplexity,
        (),
        s"Contract function (${funcMaxComplexity._1}) is too complex: ${funcMaxComplexity._2} > $maxComplexity"
      )
      s = new ContractScriptImpl(version, contract, funcMaxComplexity._2)
    } yield s
  }

  case class ContractScriptImpl(stdLibVersion: StdLibVersion, expr: Contract, maxComplexity: Long) extends Script {
    override val complexity: Long = maxComplexity
    override type Expr = Contract
    override val text: String = expr.toString
    override val bytes: Coeval[ByteStr] =
      Coeval.evalOnce {
        val s = Array(0: Byte, ScriptType.Contract.toByte, stdLibVersion.toByte) ++ ContractSerDe.serialize(expr)
        ByteStr(s ++ crypto.secureHash(s).take(checksumLength))
      }
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(true)
  }

  def estimateComplexity(version: StdLibVersion, contract: Contract): Either[String, (String, Long)] = {
    import cats.implicits._
    type E[A] = Either[String, A]
    val funcsWithComplexity: Seq[E[(String, Long)]] =
      (contract.cfs.map(func => (func.annotation.invocationArgName, func.u)) ++ contract.vf.map(func => (func.annotation.invocationArgName, func.u)))
        .map {
          case (annotationArgName, funcExpr) =>
            ScriptEstimator(varNames(version), functionCosts(version), constructExprFromFuncAndContex(contract.dec, annotationArgName, funcExpr))
              .map(complexity => (funcExpr.name, complexity))
        }
    val funcsWithComplexityEi: E[Vector[(String, Long)]] = funcsWithComplexity.toVector.sequence

    funcsWithComplexityEi.map(namesAndComp => namesAndComp.maxBy(_._2))
  }

  private def constructExprFromFuncAndContex(dec: List[DECLARATION], annotationArgName: String, funcExpr: FUNC): EXPR = {
    val funcWithAnnotationContext =
      BLOCK(
        LET(annotationArgName, TRUE),
        BLOCK(
          funcExpr,
          FUNCTION_CALL(FunctionHeader.User(funcExpr.name), List.fill(funcExpr.args.size)(TRUE))
        )
      )
    val res = dec.foldRight(funcWithAnnotationContext)((d, e) => BLOCK(d, e))
    res
  }
}
