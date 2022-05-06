package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{Asset, Call, Expression, Library, ScriptType, StdLibVersion, V3, DApp as DAppType}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.BaseGlobal.DAppInfo
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{CompilationError, Types}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

sealed trait CompileAndParseResult

object CompileAndParseResult {

  case class Expression(bytes: Array[Byte], complexity: Long, expr: Expressions.SCRIPT, errors: Seq[CompilationError]) extends CompileAndParseResult

  case class Library(bytes: Array[Byte], complexity: Long, expr: EXPR) extends CompileAndParseResult

  case class Contract(
      bytes: Array[Byte],
      verifierComplexity: Long,
      callableComplexities: Map[String, Long],
      expr: Expressions.DAPP,
      errors: Seq[CompilationError]
  ) extends CompileAndParseResult
}

sealed trait CompileResult {
  def bytes: Array[Byte]
  def verifierComplexity: Long
  def callableComplexities: Map[String, Long]
  def maxComplexity: Long
}
object CompileResult {
  case class Expression(bytes: Array[Byte], maxComplexity: Long, expr: EXPR, error: Either[String, Unit], isFreeCall: Boolean) extends CompileResult {
    override val callableComplexities: Map[String, Long] = Map.empty
    override val verifierComplexity: Long                = if (isFreeCall) 0 else maxComplexity
  }

  case class Library(bytes: Array[Byte], complexity: Long, expr: EXPR) extends CompileResult {
    override val verifierComplexity: Long                = 0
    override val callableComplexities: Map[String, Long] = Map.empty
    override val maxComplexity: Long                     = complexity
  }

  case class DApp(dAppInfo: DAppInfo, error: Either[String, Unit]) extends CompileResult {
    override def bytes: Array[Byte]                      = dAppInfo.bytes
    override def verifierComplexity: Long                = dAppInfo.verifierComplexity
    override def callableComplexities: Map[String, Long] = dAppInfo.callableComplexities
    override val maxComplexity: Long                     = callableComplexities.values.maxOption.fold(verifierComplexity)(_.max(verifierComplexity))
  }
}

object API {
  private val G: BaseGlobal = Global
  private def wavesContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean) =
    WavesContext.build(
      G,
      DirectiveSet(v, ScriptType.isAssetScript(isTokenContext), if (isContract) DAppType else Expression)
        .explicitGet()
    )

  private def cryptoContext(version: StdLibVersion) = CryptoContext.build(G, version).withEnvironment[Environment]
  private def pureContext(version: StdLibVersion) =
    PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment]

  private val fullDAppContext: Map[StdLibVersion, CTX[Environment]] =
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .map(v => (v, buildContractContext(v)))
      .toMap

  private def buildScriptContext(v: StdLibVersion, isTokenContext: Boolean, isContract: Boolean): CTX[Environment] =
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, isTokenContext, isContract)))

  private def buildContractContext(v: StdLibVersion): CTX[Environment] =
    Monoid.combineAll(Seq(pureContext(v), cryptoContext(v), wavesContext(v, false, true)))

  private val allEstimators: Seq[ScriptEstimator] = ScriptEstimator.all(fixOverflow = true)

  def latestEstimatorVersion: Int = allEstimators.length

  def allTypes(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[Types.FINAL] =
    API.buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract).types

  def allVars(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[(String, Types.FINAL)] =
    API
      .buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract)
      .vars
      .collect {
        case (name, (t, _)) if !name.startsWith("_") => (name, t)
      }
      .toSeq

  def allFunctions(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[(String, Seq[String], FunctionTypeSignature)] =
    API
      .buildScriptContext(DirectiveDictionary[StdLibVersion].idMap(ver), isTokenContext, isContract)
      .functions
      .collect {
        case f if !f.name.startsWith("_") => (f.name, f.args, f.signature)
      }
      .toSeq

  def parseAndCompile(
      input: String,
      estimatorVersion: Int = latestEstimatorVersion,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Map[String, String] = Map.empty
  ): Either[String, CompileAndParseResult] =
    for {
      estimatorVer <- Either.cond(
        estimatorVersion > 0 && estimatorVersion <= API.allEstimators.length,
        estimatorVersion,
        s"Version of estimator must be not greater than ${API.allEstimators.length}"
      )
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives)
      linkedInput <- ScriptPreprocessor(input, libraries, ds.imports)
      compiled    <- parseAndCompileScript(ds, linkedInput, API.allEstimators.toIndexedSeq(estimatorVer - 1), needCompaction, removeUnusedCode)
    } yield compiled

  private def parseAndCompileScript(
      ds: DirectiveSet,
      input: String,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean
  ): Either[String, CompileAndParseResult] = {
    val stdLibVer = ds.stdLibVersion
    val isAsset   = ds.scriptType == Asset
    ds.contentType match {
      case Expression =>
        G.parseAndCompileExpression(
          input,
          API.buildScriptContext(stdLibVer, isAsset, ds.contentType == DAppType).compilerContext,
          G.LetBlockVersions.contains(stdLibVer),
          stdLibVer,
          estimator
        ).map { case (bytes, complexity, exprScript, errors) =>
          CompileAndParseResult.Expression(bytes, complexity, exprScript, errors.toSeq)
        }
      case Library =>
        G.compileDecls(
          input,
          API.buildScriptContext(stdLibVer, isAsset, ds.contentType == DAppType).compilerContext,
          stdLibVer,
          ds.scriptType,
          estimator
        ).map { case (bytes, expr, complexity) =>
          CompileAndParseResult.Library(bytes, complexity, expr)
        }

      case DAppType =>
        G.parseAndCompileContract(
          input,
          API.fullDAppContext(ds.stdLibVersion).compilerContext,
          stdLibVer,
          estimator,
          needCompaction,
          removeUnusedCode
        ).map { case (bytes, (verifierComplexity, callableComplexities), dapp, errors) =>
          CompileAndParseResult.Contract(bytes, verifierComplexity, callableComplexities, dapp, errors.toSeq)
        }

    }
  }

  def compile(
      input: String,
      estimatorVersion: Int = latestEstimatorVersion,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Map[String, String] = Map.empty,
      defaultStdLib: StdLibVersion = StdLibVersion.VersionDic.default,
      allowFreeCall: Boolean = true
  ): Either[String, CompileResult] =
    for {
      estimatorVer <- Either.cond(
        estimatorVersion > 0 && estimatorVersion <= API.allEstimators.length,
        estimatorVersion,
        s"Version of estimator must be not greater than ${API.allEstimators.length}"
      )
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives, defaultStdLib)
      linkedInput <- ScriptPreprocessor(input, libraries, ds.imports)
      compiled    <- compileScript(ds, linkedInput, API.allEstimators.toIndexedSeq(estimatorVer - 1), needCompaction, removeUnusedCode, allowFreeCall)
    } yield compiled

  private def compileScript(
      ds: DirectiveSet,
      input: String,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean,
      allowFreeCall: Boolean
  ): Either[String, CompileResult] = {
    val version = ds.stdLibVersion
    val isAsset = ds.scriptType == Asset
    (ds.contentType, ds.scriptType) match {
      case (Expression, Call) if allowFreeCall =>
        G.compileFreeCall(input, utils.compilerContext(ds), version, ds.scriptType, estimator)
          .map { case (bytes, expr, complexity) =>
            CompileResult.Expression(bytes, complexity, expr, G.checkExpr(expr, complexity, version, ds.scriptType, estimator), isFreeCall = true)
          }
      case (Expression, Call) =>
        Left("Invoke Expression Transaction is not activated yet")
      case (Expression, _) =>
        G.compileExpression(
          input,
          API.buildScriptContext(version, isAsset, ds.contentType == DAppType).compilerContext,
          version,
          ds.scriptType,
          estimator
        ).map { case (bytes, expr, complexity) =>
          CompileResult.Expression(bytes, complexity, expr, G.checkExpr(expr, complexity, version, ds.scriptType, estimator), isFreeCall = false)

        }
      case (Library, _) =>
        G.compileDecls(input, API.buildScriptContext(version, isAsset, ds.contentType == DAppType).compilerContext, version, ds.scriptType, estimator)
          .map { case (bytes, expr, complexity) =>
            CompileResult.Library(bytes, complexity, expr)
          }
      case (DAppType, _) =>
        // Just ignore stdlib version here
        G.compileContract(input, API.fullDAppContext(ds.stdLibVersion).compilerContext, version, estimator, needCompaction, removeUnusedCode)
          .map { di =>
            CompileResult.DApp(di, G.checkContract(version, di.dApp, di.maxComplexity, di.annotatedComplexities, estimator))
          }
    }
  }
}
