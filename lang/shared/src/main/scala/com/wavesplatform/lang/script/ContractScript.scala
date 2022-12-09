package com.wavesplatform.lang.script

import cats.instances.either.*
import cats.instances.list.*
import cats.instances.option.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.VerifierFunction
import com.wavesplatform.lang.contract.meta.MetaMapper
import com.wavesplatform.lang.directives.values.{StdLibVersion, V6, DApp as DAppType}
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.ContractLimits.*
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.estimator.v3.{DAppEstimation, GlobalDeclarationsCosts, ScriptEstimatorV3}
import com.wavesplatform.lang.v1.{BaseGlobal, FunctionHeader}
import monix.eval.Coeval

object ContractScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(
      bs.length <= MaxContractSizeInBytesV6,
      (),
      s"Script is too large: ${bs.length} bytes > $MaxContractSizeInBytesV6 bytes"
    )

  def apply(version: StdLibVersion, contract: DApp): Either[String, ContractScriptImpl] =
    ContractScriptImpl(version, contract)
      .asRight[String]
      .flatTap(s => validateBytes(s.bytes().arr))

  case class ContractScriptImpl(stdLibVersion: StdLibVersion, expr: DApp) extends Script {
    override type Expr = DApp
    override val isFreeCall: Boolean = false
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

    def isUnionInCallableAllowed: Either[String, Boolean] =
      if (stdLibVersion < V6) {
        Right(true)
      } else {
        MetaMapper
          .dicFromProto(expr)
          .map(!_.callableFuncTypes.exists(_.flatten.exists(_.containsUnion)))
      }
  }

  private def estimateAnnotatedFunctions(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean
  ): Either[String, Iterable[(String, Long)]] =
    for {
      callables <- estimateDeclarations(version, dApp, estimator, callables(dApp), preserveDefinition = true)
      verifier  <- dApp.verifierFuncOpt.traverse(estimateVerifier(version, dApp, estimator, fixEstimateOfVerifier, _))
    } yield verifier ++ callables

  private def callables(dApp: DApp): List[(Some[String], FUNC)] =
    dApp.callableFuncs
      .map(func => (Some(func.annotation.invocationArgName), func.u))

  def estimateDeclarations(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      declarations: List[(Option[String], DECLARATION)],
      preserveDefinition: Boolean
  ): Either[String, List[(String, Long)]] =
    declarations
      .traverse { case (annotationArgName, funcExpr) =>
        estimator(
          varNames(version, DAppType),
          functionCosts(version, DAppType),
          constructExprFromDeclAndContext(dApp.decs, annotationArgName, funcExpr, preserveDefinition)
        ).map((funcExpr.name, _))
      }

  private def estimateVerifier(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean,
      verifier: VerifierFunction
  ): Either[String, (String, Long)] = {
    if (dApp.verifierContainsSyncCall)
      Left("DApp-to-dApp invocations are not allowed from verifier")
    else
      estimator(
        varNames(version, DAppType),
        functionCosts(version, DAppType, isDAppVerifier = !fixEstimateOfVerifier),
        constructExprFromDeclAndContext(dApp.decs, Some(verifier.annotation.invocationArgName), verifier.u, preserveDefinition = true)
      ).map((verifier.u.name, _))
  }

  private[script] def constructExprFromDeclAndContext(
      dec: List[DECLARATION],
      annotationArgNameOpt: Option[String],
      decl: DECLARATION,
      preserveDefinition: Boolean
  ): EXPR = {
    val declExpr =
      decl match {
        case let @ LET(name, _) if preserveDefinition =>
          BLOCK(let, REF(name))
        case LET(name, _) =>
          REF(name)
        case func @ FUNC(name, args, _) if preserveDefinition =>
          BLOCK(
            func,
            FUNCTION_CALL(FunctionHeader.User(name), List.fill(args.size)(TRUE))
          )
        case FUNC(name, args, _) =>
          FUNCTION_CALL(FunctionHeader.User(name), List.fill(args.size)(TRUE))
        case Terms.FAILED_DEC() =>
          FAILED_EXPR()
      }
    val funcWithContext = annotationArgNameOpt.fold(declExpr)(argName => BLOCK(LET(argName, TRUE), declExpr))
    dec.foldRight(funcWithContext)((declaration, expr) => BLOCK(declaration, expr))
  }

  def estimateComplexity(
      version: StdLibVersion,
      dApp: DApp,
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean,
      useReducedVerifierLimit: Boolean = true
  ): Either[String, (Long, Map[String, Long])] =
    for {
      (maxComplexity, complexities) <- estimateComplexityExact(version, dApp, estimator, fixEstimateOfVerifier)
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
      estimator: ScriptEstimator,
      fixEstimateOfVerifier: Boolean
  ): Either[String, ((String, Long), Map[String, Long])] =
    for {
      annotatedFunctionComplexities <- estimateAnnotatedFunctions(version, dApp, estimator, fixEstimateOfVerifier)
      max = annotatedFunctionComplexities.toList.maximumOption(_._2 compareTo _._2).getOrElse(("", 0L))
    } yield (max, annotatedFunctionComplexities.toMap)

  def estimateFully(version: StdLibVersion, dApp: DApp, estimator: ScriptEstimator): Either[String, DAppEstimation] = {
    estimator match {
      case estimatorV3: ScriptEstimatorV3 =>
        val allDecs   = dApp.decs ++ dApp.callableFuncs.map(_.u) ++ dApp.verifierFuncOpt.map(_.u)
        val singleAst = allDecs.foldRight[EXPR](TRUE) { case (decl, expr) => BLOCK(decl, expr) }
        val libCosts  = functionCosts(version, DAppType)
        for {
          GlobalDeclarationsCosts(lets, functions) <- estimatorV3.globalDeclarationsCosts(libCosts, singleAst)
          nonAnnotatedFunctions = functions.view.filterKeys(k => dApp.decs.exists(_.name == k)).toMap
          annotatedFunctions    = functions.view.filterKeys(!nonAnnotatedFunctions.contains(_)).toMap
        } yield DAppEstimation(annotatedFunctions, lets, nonAnnotatedFunctions)
      case oldEstimator =>
        for {
          annotatedComplexities <- estimateAnnotatedFunctions(version, dApp, oldEstimator, fixEstimateOfVerifier = true)
          letsCosts             <- oldGlobalLetsCosts(version, dApp, oldEstimator)
          functionsCosts        <- oldGlobalFunctionsCosts(version, dApp, oldEstimator)
        } yield DAppEstimation(annotatedComplexities.toMap, letsCosts, functionsCosts)
    }
  }

  def oldGlobalFunctionsCosts(version: StdLibVersion, dApp: DApp, estimator: ScriptEstimator): Either[String, Map[String, Long]] =
    ContractScript
      .estimateDeclarations(version, dApp, estimator, dApp.decs.collect { case f: FUNC => (None, f) }, preserveDefinition = false)
      .map(_.toMap)

  def oldGlobalLetsCosts(version: StdLibVersion, dApp: DApp, estimator: ScriptEstimator): Either[String, Map[String, Long]] =
    ContractScript
      .estimateDeclarations(version, dApp, estimator, dApp.decs.collect { case l: LET => (None, l) }, preserveDefinition = false)
      .map(_.toMap)
}
