package com.wavesplatform.lang.script

import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.directives.values.{StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import monix.eval.Coeval

object ContractScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
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

  private def estimateAnnotatedFunctions(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, Iterable[(String, Long)]] =
    for {
      callables <- estimateDeclarations(version, dApp, estimator, callables(dApp))
      verifier  <- dApp.verifierFuncOpt.traverse(estimateVerifier(version, dApp, estimator, _))
    } yield verifier ++ callables

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

  private def callables(dApp: DApp): List[(Some[String], FUNC)] =
    dApp.callableFuncs
      .map(func => (Some(func.annotation.invocationArgName), func.u))

  private def estimateDeclarations(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      functions: List[(Option[String], DECLARATION)]
  ): Either[String, List[(String, Long)]] =
    functions.traverse {
      case (annotationArgName, funcExpr) =>
        estimator(
          varNames(version, DAppType),
          functionCosts(version, DAppType),
          constructExprFromDeclAndContext(dApp.decs, annotationArgName, funcExpr)
        ).map((funcExpr.name, _))
    }

  private def estimateVerifier(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      verifier: VerifierFunction
  ): Either[String, (String, Long)] =
    estimator(
      varNames(version, DAppType),
      functionCosts(version, DAppType, isDAppVerifier = true),
      constructExprFromDeclAndContext(dApp.decs, Some(verifier.annotation.invocationArgName), verifier.u)
    ).map((verifier.u.name, _))

  private[script] def constructExprFromDeclAndContext(
      dec: List[DECLARATION],
      annotationArgNameOpt: Option[String],
      decl: DECLARATION
  ): EXPR = {
    val declExpr =
      decl match {
        case let @ LET(name, _) =>
          BLOCK(let, REF(name))
        case func @ FUNC(name, args, _) =>
          BLOCK(
            func,
            FUNCTION_CALL(FunctionHeader.User(name), List.fill(args.size)(TRUE))
          )
        case Terms.FAILED_DEC() =>
          FAILED_EXPR()
      }
    val funcWithContext =
      annotationArgNameOpt.fold(declExpr)(
        annotationArgName => BLOCK(LET(annotationArgName, TRUE), declExpr)
      )
    dec.foldRight(funcWithContext)((declaration, expr) => BLOCK(declaration, expr))
  }

  def estimateComplexity(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      useReducedVerifierLimit: Boolean = true
  ): Either[String, (Long, Map[String, Long])] =
    for {
      (maxComplexity, complexities) <- estimateComplexityExact(version, dApp, estimator)
      _                             <- checkComplexity(version, dApp, maxComplexity, complexities, useReducedVerifierLimit)
    } yield (maxComplexity._2, complexities)

  def checkComplexity(
      version: StdLibVersion,
      dApp: DApp,
      maxComplexity: (String, Long),
      complexities: Map[String, Long],
      useReducedVerifierLimit: Boolean
  ): Either[String, Unit] =
    for {
      _ <- if (useReducedVerifierLimit) estimateVerifierReduced(dApp, complexities, version) else Right(())
      limit = MaxCallableComplexityByVersion(version)
      _ <- Either.cond(
        maxComplexity._2 <= limit,
        (),
        s"Contract function (${maxComplexity._1}) is too complex: ${maxComplexity._2} > $limit"
      )
    } yield ()

  private def estimateVerifierReduced(
      dApp: DApp,
      complexities: Map[String, Long],
      version: StdLibVersion
  ): Either[String, Unit] =
    dApp.verifierFuncOpt.fold(().asRight[String]) { verifier =>
      val verifierComplexity = complexities(verifier.u.name)
      val limit              = MaxAccountVerifierComplexityByVersion(version)
      Either.cond(
        verifierComplexity <= limit,
        (),
        s"Contract verifier is too complex: $verifierComplexity > $limit"
      )
    }

  def estimateComplexityExact(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator
  ): Either[String, ((String, Long), Map[String, Long])] =
    for {
      annotatedFunctionComplexities <- estimateAnnotatedFunctions(version, dApp, estimator)
      max = annotatedFunctionComplexities.toList.maximumOption(_._2 compareTo _._2).getOrElse(("", 0L))
    } yield (max, annotatedFunctionComplexities.toMap)
}
