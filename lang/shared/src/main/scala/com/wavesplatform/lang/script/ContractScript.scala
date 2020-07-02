package com.wavesplatform.lang.script

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits._
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
  }

  private def estimateAnnotatedFunctions(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, List[(String, Long)]] =
    estimateDeclarations(version, dApp, estimator, annotatedFunctions(dApp))

  def estimateUserFunctions(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, List[(String, Long)]] =
    estimateDeclarations(version, dApp, estimator, dApp.decs.collect { case f: FUNC => (None, f) })

  def estimateGlobalVariables(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, List[(String, Long)]] =
    estimateDeclarations(version, dApp, estimator, dApp.decs.collect { case l: LET => (None, l) })

  private def annotatedFunctions(dApp: DApp): List[(Some[String], FUNC)] =
    (dApp.verifierFuncOpt ++ dApp.callableFuncs)
      .map(func => (Some(func.annotation.invocationArgName), func.u))
      .toList

  private def estimateDeclarations(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      declarations: List[(Option[String], DECLARATION)]
  ): Either[String, List[(String, Long)]] =
    declarations.traverse {
      case (annotationArgName, funcExpr) =>
        estimator(
          varNames(version, DAppType),
          functionCosts(version),
          constructExprFromDeclAndContext(dApp.decs, annotationArgName, funcExpr)
        ).map((funcExpr.name, _))
    }

  private[script] def constructExprFromDeclAndContext(
    dec: List[DECLARATION],
    annotationArgNameOpt: Option[String],
    decl: DECLARATION
  ): EXPR = {
    val declExpr =
      decl match {
        case let@LET(name, _) =>
          BLOCK(let, REF(name))
        case func@FUNC(name, args, _) =>
          BLOCK(
            func,
            FUNCTION_CALL(FunctionHeader.User(name), List.fill(args.size)(TRUE))
          )
      }

    val funcWithContext =
      annotationArgNameOpt.fold(declExpr)(
        annotationArgName =>
          BLOCK(LET(annotationArgName, TRUE), declExpr)
      )
    dec.foldRight(funcWithContext)((declaration, expr) => BLOCK(declaration, expr))
  }

  def estimateComplexity(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, (Long, Map[String, Long])] =
    for {
      (maxComplexity, complexities) <- estimateComplexityExact(version, dApp, estimator)
      _                             <- checkComplexity(version, maxComplexity)
    } yield (maxComplexity._2, complexities)

  def checkComplexity(
      version: StdLibVersion,
      maxComplexity: (String, Long)
  ): Either[String, Unit] = {
    val limit = MaxComplexityByVersion(version)
    Either.cond(
      maxComplexity._2 <= limit,
      (),
      s"Contract function (${maxComplexity._1}) is too complex: ${maxComplexity._2} > $limit"
    )
  }

  def estimateComplexityExact(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, ((String, Long), Map[String, Long])] =
    for {
      annotatedFunctionComplexities <- estimateAnnotatedFunctions(version, dApp, estimator)
      max = annotatedFunctionComplexities.maximumOption(_._2 compareTo _._2).getOrElse(("", 0L))
    } yield (max, annotatedFunctionComplexities.toMap)
}
