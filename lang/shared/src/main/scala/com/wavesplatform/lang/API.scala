package com.wavesplatform.lang

import com.wavesplatform.lang.directives.Directive.extractDirectives
import com.wavesplatform.lang.directives.values.{Call, Expression, Library, StdLibVersion, DApp as DAppType}
import com.wavesplatform.lang.directives.{DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.BaseGlobal.DAppInfo
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{CompilationError, Types}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.evaluator.ctx.FunctionTypeSignature
import com.wavesplatform.lang.v1.parser.Expressions

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
  val version: StdLibVersion
  def bytes: Array[Byte]
  def verifierComplexity: Long
  def callableComplexities: Map[String, Long]
  def maxComplexity: Long
}
object CompileResult {
  case class Expression(version: StdLibVersion, bytes: Array[Byte], maxComplexity: Long, expr: EXPR, error: Either[String, Unit], isFreeCall: Boolean)
      extends CompileResult {
    override val callableComplexities: Map[String, Long] = Map.empty
    override val verifierComplexity: Long                = if (isFreeCall) 0 else maxComplexity
  }

  case class Library(version: StdLibVersion, bytes: Array[Byte], complexity: Long, expr: EXPR) extends CompileResult {
    override val verifierComplexity: Long                = 0
    override val callableComplexities: Map[String, Long] = Map.empty
    override val maxComplexity: Long                     = complexity
  }

  case class DApp(version: StdLibVersion, dAppInfo: DAppInfo, error: Either[String, Unit]) extends CompileResult {
    override def bytes: Array[Byte]                      = dAppInfo.bytes
    override def verifierComplexity: Long                = dAppInfo.verifierComplexity
    override def callableComplexities: Map[String, Long] = dAppInfo.callableComplexities
    override val maxComplexity: Long                     = callableComplexities.values.maxOption.fold(verifierComplexity)(_.max(verifierComplexity))
  }
}

object API {
  private val G: BaseGlobal                       = Global
  private val allEstimators: Seq[ScriptEstimator] = ScriptEstimator.all(fixOverflow = true)

  def latestEstimatorVersion: Int = allEstimators.length

  def allTypes(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[Types.FINAL] =
    utils.ctx(ver, isTokenContext, isContract).types

  def allVars(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[(String, Types.FINAL)] =
    utils
      .ctx(ver, isTokenContext, isContract)
      .vars
      .collect {
        case (name, (t, _)) if !name.startsWith("_") => (name, t)
      }
      .toSeq

  def allFunctions(ver: Int = 2, isTokenContext: Boolean = false, isContract: Boolean = false): Seq[(String, Seq[String], FunctionTypeSignature)] =
    utils
      .ctx(ver, isTokenContext, isContract)
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
    ds.contentType match {
      case Expression =>
        G.parseAndCompileExpression(
          input,
          utils.compilerContext(ds),
          G.LetBlockVersions.contains(stdLibVer),
          stdLibVer,
          estimator
        ).map { case (bytes, complexity, exprScript, errors) =>
          CompileAndParseResult.Expression(bytes, complexity, exprScript, errors.toSeq)
        }
      case Library =>
        G.compileDecls(
          input,
          utils.compilerContext(ds),
          stdLibVer,
          ds.scriptType,
          estimator
        ).map { case (bytes, expr, complexity) =>
          CompileAndParseResult.Library(bytes, complexity, expr)
        }

      case DAppType =>
        G.parseAndCompileContract(
          input,
          utils.compilerContext(ds),
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
      estimator: ScriptEstimator,
      needCompaction: Boolean = false,
      removeUnusedCode: Boolean = false,
      libraries: Map[String, String] = Map.empty,
      defaultStdLib: StdLibVersion = StdLibVersion.VersionDic.default,
      allowFreeCall: Boolean = true
  ): Either[String, CompileResult] =
    for {
      directives  <- DirectiveParser(input)
      ds          <- extractDirectives(directives, defaultStdLib)
      linkedInput <- ScriptPreprocessor(input, libraries, ds.imports)
      compiled    <- compileScript(ds, linkedInput, estimator, needCompaction, removeUnusedCode, allowFreeCall)
    } yield compiled

  def estimatorByVersion(version: Int): Either[String, ScriptEstimator] =
    Either
      .cond(
        version > 0 && version <= allEstimators.length,
        version,
        s"Version of estimator must be not greater than ${API.allEstimators.length}"
      )
      .map(v => allEstimators.toIndexedSeq(v - 1))

  private def compileScript(
      ds: DirectiveSet,
      input: String,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean,
      allowFreeCall: Boolean
  ): Either[String, CompileResult] = {
    val version = ds.stdLibVersion
    val ctx     = utils.compilerContext(ds)
    (ds.contentType, ds.scriptType) match {
      case (Expression, Call) if allowFreeCall =>
        G.compileFreeCall(input, ctx, version, ds.scriptType, estimator)
          .map { case (bytes, expr, complexity) =>
            val check = G.checkExpr(expr, complexity, version, ds.scriptType, estimator)
            CompileResult.Expression(version, bytes, complexity, expr, check, isFreeCall = true)
          }
      case (Expression, Call) =>
        Left("Invoke Expression Transaction is not activated yet")
      case (Expression, _) =>
        G.compileExpression(input, ctx, version, ds.scriptType, estimator)
          .map { case (bytes, expr, complexity) =>
            val check = G.checkExpr(expr, complexity, version, ds.scriptType, estimator)
            CompileResult.Expression(version, bytes, complexity, expr, check, isFreeCall = false)
          }
      case (Library, _) =>
        G.compileDecls(input, ctx, version, ds.scriptType, estimator)
          .map { case (bytes, expr, complexity) =>
            CompileResult.Library(version, bytes, complexity, expr)
          }
      case (DAppType, _) =>
        // Just ignore stdlib version here
        G.compileContract(input, ctx, version, estimator, needCompaction, removeUnusedCode)
          .map { di =>
            CompileResult.DApp(version, di, G.checkContract(version, di.dApp, di.maxComplexity, di.annotatedComplexities, estimator))
          }
    }
  }
}
