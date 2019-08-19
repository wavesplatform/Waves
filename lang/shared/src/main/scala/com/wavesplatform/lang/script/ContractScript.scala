package com.wavesplatform.lang.script
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits.{MaxComplexityByVersion, MaxContractSizeInBytes}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader, ScriptEstimator}
import monix.eval.Coeval
import cats.implicits._

object ContractScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= MaxContractSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $MaxContractSizeInBytes bytes")

  def apply(version: StdLibVersion, contract: DApp): Either[String, Script] = {
    for {
      funcMaxComplexity <- estimateComplexityByFunction(version, contract)
      tcf = funcMaxComplexity.find(_._2 > MaxComplexityByVersion(version))
      _ <- Either.cond(
        tcf.isEmpty,
        (),
        s"Contract function (${tcf.get._1}) is too complex: ${tcf.get._2} > ${MaxComplexityByVersion(version)}"
      )
      s = ContractScriptImpl(version, contract, funcMaxComplexity.toMap)
      _ <- validateBytes(s.bytes().arr)

    } yield s
  }

  case class ContractScriptImpl(stdLibVersion: StdLibVersion, expr: DApp, complexityMap: Map[String, Long]) extends Script {
    override val complexity: Long = (0L +: complexityMap.toSeq.map(_._2)).max
    override type Expr = DApp
    override val bytes: Coeval[ByteStr] = Coeval.fromTry(
      Global.serializeContract(expr, stdLibVersion)
        .bimap(new RuntimeException(_), ByteStr(_))
        .toTry
    )
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(true)
  }

  def estimateComplexityByFunction(version: StdLibVersion, contract: DApp): Either[String, Vector[(String, Long)]] = {
    import cats.implicits._
    val funcsWithComplexity: Seq[Either[String, (String, Long)]] =
      (contract.callableFuncs.map(func => (func.annotation.invocationArgName, func.u)) ++
        contract.verifierFuncOpt.map(func => (func.annotation.invocationArgName, func.u)))
        .map {
          case (annotationArgName, funcExpr) =>
            ScriptEstimator(varNames(version, DAppType),
                            functionCosts(version),
                            constructExprFromFuncAndContext(contract.decs, annotationArgName, funcExpr))
              .map(complexity => (funcExpr.name, complexity))
        }
    funcsWithComplexity.toVector.sequence
  }

  def estimateComplexity(version: StdLibVersion, contract: DApp): Either[String, (Long, Vector[(String, Long)])] =
    estimateComplexityByFunction(version, contract)
      .map(namesAndComp => ((("", 0L) +: namesAndComp).map(_._2).max, namesAndComp))

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
