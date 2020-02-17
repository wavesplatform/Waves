package com.wavesplatform.lang.script

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits.{MaxComplexityByVersion, MaxContractSizeInBytes}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import monix.eval.Coeval

object ContractScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  private def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(
      bs.length <= MaxContractSizeInBytes,
      (),
      s"Script is too large: ${bs.length} bytes > $MaxContractSizeInBytes bytes"
    )

  def apply(version: StdLibVersion, contract: DApp): Either[String, Script] =
    ContractScriptImpl(version, contract)
      .asRight[String]
      .flatTap(s => validateBytes(s.bytes().arr))

  case class ContractScriptImpl(stdLibVersion: StdLibVersion, expr: DApp) extends Script {
    override type Expr = DApp
    override val bytes: Coeval[ByteStr] = Coeval.fromTry(
      Global
        .serializeContract(expr, stdLibVersion)
        .bimap(new RuntimeException(_), ByteStr(_))
        .toTry
    )
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(true)

    override val containsArray: Boolean = {
      val declExprs = expr.decs.map {
        case l: LET        => l.value
        case f: FUNC       => f.body
        case _: FAILED_DEC => FAILED_EXPR()
      }
      val callableExprs = expr.callableFuncs.map(_.u.body)
      val verifierExpr  = expr.verifierFuncOpt.map(_.u.body).toList

      (verifierExpr ::: declExprs ::: callableExprs)
        .exists(com.wavesplatform.lang.v1.compiler.containsArray)
    }
  }

  def estimateComplexityByFunction(
      version: StdLibVersion,
      contract: DApp,
      estimator: ScriptEstimator
  ): Either[String, List[(String, Long)]] =
    (contract.callableFuncs.map(func => (func.annotation.invocationArgName, func.u)) ++
      contract.verifierFuncOpt.map(func => (func.annotation.invocationArgName, func.u)))
      .traverse {
        case (annotationArgName, funcExpr) =>
          estimator(
            varNames(version, DAppType),
            functionCosts(DirectiveSet(version, Account, DAppType).explicitGet()),
            constructExprFromFuncAndContext(contract.decs, annotationArgName, funcExpr)
          ).map((funcExpr.name, _))
      }

  def estimateComplexity(
      version: StdLibVersion,
      contract: DApp,
      estimator: ScriptEstimator,
    checkLimit: Boolean = true
  ): Either[String, (Long, Map[String, Long])] =
    for {
      cbf <- estimateComplexityByFunction(version, contract, estimator)
      max = cbf.maximumOption(_._2 compareTo _._2)
      _ <- max.fold(().asRight[String])(
        m =>
          Either.cond(
            !(checkLimit && m._2 > MaxComplexityByVersion(version)),
            (),
            s"Contract function (${m._1}) is too complex: ${m._2} > ${MaxComplexityByVersion(version)}"
          )
      )
    } yield (max.map(_._2).getOrElse(0L), cbf.toMap)

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
