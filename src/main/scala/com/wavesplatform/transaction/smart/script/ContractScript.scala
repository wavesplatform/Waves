package com.wavesplatform.transaction.smart.script
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.{ContentType, Global}
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.{FunctionHeader, ScriptEstimator}
import com.wavesplatform.utils.{functionCosts, varNames}
import monix.eval.Coeval

object ContractScript {

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= MaxContractSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $MaxContractSizeInBytes bytes")

  def apply(version: StdLibVersion, contract: DApp): Either[String, Script] = {
    for {
      funcMaxComplexity <- estimateComplexity(version, contract)
      _ <- Either.cond(
        funcMaxComplexity._2 <= MaxContractComplexity,
        (),
        s"Contract function (${funcMaxComplexity._1}) is too complex: ${funcMaxComplexity._2} > $MaxContractComplexity"
      )
      s = ContractScriptImpl(version, contract, funcMaxComplexity._2)
      _ <- validateBytes(s.bytes().arr)

    } yield s
  }

  case class ContractScriptImpl(stdLibVersion: StdLibVersion, expr: DApp, maxComplexity: Long) extends Script {
    override val complexity: Long = maxComplexity
    override type Expr = DApp
    override val bytes: Coeval[ByteStr]           = Coeval.evalOnce(ByteStr(Global.serializeContract(expr, stdLibVersion)))
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(true)
  }

  def estimateComplexity(version: StdLibVersion, contract: DApp): Either[String, (String, Long)] = {
    import cats.implicits._
    type E[A] = Either[String, A]
    val funcsWithComplexity: Seq[E[(String, Long)]] =
      (contract.cfs.map(func => (func.annotation.invocationArgName, func.u)) ++ contract.vf.map(func => (func.annotation.invocationArgName, func.u)))
        .map {
          case (annotationArgName, funcExpr) =>
            ScriptEstimator(varNames(version, ContentType.DApp),
                            functionCosts(version),
                            constructExprFromFuncAndContext(contract.dec, annotationArgName, funcExpr))
              .map(complexity => (funcExpr.name, complexity))
        }
    val funcsWithComplexityEi: E[Vector[(String, Long)]] = funcsWithComplexity.toVector.sequence

    funcsWithComplexityEi.map(namesAndComp => (("", 0L) +: namesAndComp).maxBy(_._2))
  }

  private def constructExprFromFuncAndContext(dec: List[DECLARATION], annotationArgName: String, funcExpr: FUNC): EXPR = {
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
