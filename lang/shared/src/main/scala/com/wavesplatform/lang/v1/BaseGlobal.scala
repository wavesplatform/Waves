package com.wavesplatform.lang.v1

import java.math.RoundingMode

import cats.implicits._
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.meta.{FunctionSignatures, MetaMapper, ParsedMeta}
import com.wavesplatform.lang.contract.{ContractSerDe, DApp}
import com.wavesplatform.lang.directives.values.{Expression, StdLibVersion, V1, V2, V5, DApp => DAppType}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.BaseGlobal.{ArrayView, DAppInfo}
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.compiler.{CompilationError, CompilerContext, ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.repl.node.http.response.model.NodeResponse

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.util.Random

/**
  * This is a hack class for IDEA. The Global class is in JS/JVM modules.
  * And IDEA can't find the Global class in the "shared" module, but it must!
  */
trait BaseGlobal {
  val MaxBase16Bytes               = 8 * 1024
  val MaxBase16String              = 32 * 1024
  val MaxBase58Bytes               = 64
  val MaxBase58String              = 100
  val MaxBase64Bytes               = 32 * 1024
  val MaxBase64String              = 44 * 1024
  val MaxLiteralLength             = 12 * 1024
  val MaxAddressLength             = 36
  val MaxByteStrSizeForVerifyFuncs = 32 * 1024

  val LetBlockVersions = Set[StdLibVersion](V1, V2)

  def base58Encode(input: Array[Byte]): Either[String, String]
  def base58Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def base64Encode(input: Array[Byte]): Either[String, String]
  def base64Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def base16Encode(input: Array[Byte], checkLength: Boolean): Either[String, String] =
    if (checkLength && input.length > MaxBase16Bytes)
      Left(s"Base16 encode input length=${input.length} should not exceed $MaxBase16Bytes")
    else
      base16EncodeImpl(input)

  def base16Decode(input: String, checkLength: Boolean): Either[String, Array[Byte]] =
    if (checkLength && input.length > MaxBase16String)
      Left(s"Base16 decode input length=${input.length} should not exceed $MaxBase16String")
    else
      base16DecodeImpl(input)

  protected def base16EncodeImpl(input: Array[Byte]): Either[String, String]

  protected def base16DecodeImpl(input: String): Either[String, Array[Byte]]

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

  def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Either[String, Boolean]

  def keccak256(message: Array[Byte]): Array[Byte]
  def blake2b256(message: Array[Byte]): Array[Byte]
  def sha256(message: Array[Byte]): Array[Byte]

  def secureHash(a: Array[Byte]): Array[Byte] = keccak256(blake2b256(a))

  def checksum(arr: Array[Byte]): Array[Byte] = secureHash(arr).take(4)

  def serializeExpression(expr: EXPR, stdLibVersion: StdLibVersion): Array[Byte] = {
    val s = Array(stdLibVersion.id.toByte) ++ Serde.serialize(expr)
    s ++ checksum(s)
  }

  def serializeContract(c: DApp, stdLibVersion: StdLibVersion): Either[String, Array[Byte]] =
    ContractSerDe
      .serialize(c)
      .map(Array(0: Byte, DAppType.id.toByte, stdLibVersion.id.toByte) ++ _)
      .map(r => r ++ checksum(r))

  def parseAndCompileExpression(
      input: String,
      context: CompilerContext,
      letBlockOnly: Boolean,
      stdLibVersion: StdLibVersion,
      estimator: ScriptEstimator
  ): Either[String, (Array[Byte], Long, Expressions.SCRIPT, Iterable[CompilationError])] = {
    (for {
      compRes <- ExpressionCompiler.compileWithParseResult(input, context)
      (compExpr, exprScript, compErrorList) = compRes
      illegalBlockVersionUsage              = letBlockOnly && com.wavesplatform.lang.v1.compiler.containsBlockV2(compExpr)
      _ <- Either.cond(!illegalBlockVersionUsage, (), "UserFunctions are only enabled in STDLIB_VERSION >= 3")
      bytes = if (compErrorList.isEmpty) serializeExpression(compExpr, stdLibVersion) else Array.empty[Byte]

      vars  = utils.varNames(stdLibVersion, Expression)
      costs = utils.functionCosts(stdLibVersion)
      complexity <- if (compErrorList.isEmpty) estimator(vars, costs, compExpr) else Either.right(0L)
    } yield (bytes, complexity, exprScript, compErrorList))
      .recover {
        case e => (Array.empty, 0, Expressions.SCRIPT(AnyPos, Expressions.INVALID(AnyPos, "Unknown error.")), List(Generic(0, 0, e)))
      }
  }

  def parseAndCompileContract(
      input: String,
      ctx: CompilerContext,
      stdLibVersion: StdLibVersion,
      estimator: ScriptEstimator
  ): Either[String, (Array[Byte], (Long, Map[String, Long]), Expressions.DAPP, Iterable[CompilationError])] = {
    (for {
      compRes <- ContractCompiler.compileWithParseResult(input, ctx, stdLibVersion)
      (compDAppOpt, exprDApp, compErrorList) = compRes
      complexityWithMap <- if (compDAppOpt.nonEmpty && compErrorList.isEmpty)
        ContractScript.estimateComplexity(stdLibVersion, compDAppOpt.get, estimator)
      else Right((0L, Map.empty[String, Long]))
      bytes <- if (compDAppOpt.nonEmpty && compErrorList.isEmpty) serializeContract(compDAppOpt.get, stdLibVersion) else Right(Array.empty[Byte])
    } yield (bytes, complexityWithMap, exprDApp, compErrorList))
      .recover {
        case e => (Array.empty, (0, Map.empty), Expressions.DAPP(AnyPos, List.empty, List.empty), List(Generic(0, 0, e)))
      }
  }

  val compileExpression =
    compile(_, _, _, _, ExpressionCompiler.compileBoolean)

  val compileDecls =
    compile(_, _, _, _, ExpressionCompiler.compileDecls)

  private def compile(
      input: String,
      context: CompilerContext,
      version: StdLibVersion,
      estimator: ScriptEstimator,
      compiler: (String, CompilerContext) => Either[String, EXPR]
  ): Either[String, (Array[Byte], EXPR, Long)] =
    for {
      expr <- compiler(input, context)
      bytes = serializeExpression(expr, version)
      _          <- ExprScript.validateBytes(bytes)
      complexity <- ExprScript.estimateExact(expr, version, estimator)
    } yield (bytes, expr, complexity)

  def checkExpr(
      expr: EXPR,
      complexity: Long,
      version: StdLibVersion,
      isAsset: Boolean,
      estimator: ScriptEstimator
  ): Either[String, Unit] =
    for {
      _ <- if (estimator == ScriptEstimatorV2)
        ExprScript.estimate(expr, version, ScriptEstimatorV1, !isAsset)
      else
        Right(())
      _ <- ExprScript.checkComplexity(version, complexity, !isAsset)
      illegalBlockVersionUsage = LetBlockVersions.contains(version) &&
        com.wavesplatform.lang.v1.compiler.containsBlockV2(expr)
      _ <- Either.cond(
        !illegalBlockVersionUsage,
        (),
        "UserFunctions are only enabled in STDLIB_VERSION >= 3"
      )
    } yield ()

  def compileContract(
      input: String,
      ctx: CompilerContext,
      stdLibVersion: StdLibVersion,
      estimator: ScriptEstimator
  ): Either[String, DAppInfo] =
    for {
      dApp                                   <- ContractCompiler.compile(input, ctx, stdLibVersion)
      userFunctionComplexities               <- ContractScript.estimateUserFunctions(stdLibVersion, dApp, estimator)
      globalVariableComplexities             <- ContractScript.estimateGlobalVariables(stdLibVersion, dApp, estimator)
      (maxComplexity, annotatedComplexities) <- ContractScript.estimateComplexityExact(stdLibVersion, dApp, estimator)
      stateCallsComplexities                 <- ContractScript.estimateStateCalls(stdLibVersion, dApp, dApp.callableFuncs)
      (verifierComplexity, callableComplexities) = dApp.verifierFuncOpt.fold(
        (0L, annotatedComplexities)
      )(v => (annotatedComplexities(v.u.name), annotatedComplexities - v.u.name))
      bytes <- serializeContract(dApp, stdLibVersion)
      _     <- ContractScript.validateBytes(bytes)
    } yield DAppInfo(
      bytes,
      dApp,
      maxComplexity,
      annotatedComplexities,
      verifierComplexity,
      callableComplexities,
      stateCallsComplexities.toMap,
      userFunctionComplexities.toMap,
      globalVariableComplexities.toMap
    )

  def checkContract(
      version: StdLibVersion,
      dApp: DApp,
      maxComplexity: (String, Long),
      complexities: Map[String, Long],
      estimator: ScriptEstimator
  ): Either[String, Unit] =
    for {
      _ <- if (estimator == ScriptEstimatorV2)
        ContractScript.estimateComplexity(version, dApp, ScriptEstimatorV1)
      else
        Right(())
      _ <- ContractScript.checkComplexity(
        version,
        dApp,
        maxComplexity,
        complexities,
        useReducedVerifierLimit = true,
        allowContinuation = version >= V5
      )
    } yield ()

  def decompile(compiledCode: String): Either[ScriptParseError, String] =
    Script
      .fromBase64String(compiledCode.trim)
      .map(script => Script.decompile(script)._1)

  def dAppFuncTypes(compiledCode: String): Either[ScriptParseError, FunctionSignatures] =
    for {
      script <- Script.fromBase64String(compiledCode.trim)
      result <- dAppFuncTypes(script)
    } yield result

  def dAppFuncTypes(script: Script): Either[ScriptParseError, FunctionSignatures] =
    script match {
      case ContractScriptImpl(_, dApp) =>
        MetaMapper.dicFromProto(dApp).bimap(ScriptParseError, combineMetaWithDApp(_, dApp))
      case _ => Left(ScriptParseError("Expected DApp"))
    }

  private def combineMetaWithDApp(meta: ParsedMeta, dApp: DApp): FunctionSignatures = {
    val argTypesWithFuncName =
      meta.callableFuncTypes.fold(List.empty[(String, List[(String, FINAL)])])(
        types =>
          (types zip dApp.callableFuncs)
            .map {
              case (argTypes, func) =>
                func.u.name -> (func.u.args zip argTypes)
            }
      )
    FunctionSignatures(meta.version, argTypesWithFuncName)
  }

  def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean

  // Math functions

  def pow(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds): Either[String, Long]
  def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: BaseGlobal.Rounds): Either[String, Long]

  import RoundingMode._

  protected def roundMode(round: BaseGlobal.Rounds): RoundingMode =
    round match {
      case BaseGlobal.RoundUp()       => UP
      case BaseGlobal.RoundHalfUp()   => HALF_UP
      case BaseGlobal.RoundHalfDown() => HALF_DOWN
      case BaseGlobal.RoundDown()     => DOWN
      case BaseGlobal.RoundHalfEven() => HALF_EVEN
      case BaseGlobal.RoundCeiling()  => CEILING
      case BaseGlobal.RoundFloor()    => FLOOR
    }

  def requestNode(url: String): Future[NodeResponse]

  def groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean

  def bn256Groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean

  def ecrecover(messageHash: Array[Byte], signature: Array[Byte]): Array[Byte]

  def median(seq: Seq[Long]): Long = {
    @tailrec
    def findKMedianInPlace(arr: ArrayView, k: Int)(implicit choosePivot: ArrayView => Long): Long = {
      val a      = choosePivot(arr)
      val (s, b) = arr partitionInPlace (a >)
      if (s.size == k) a
      // The following test is used to avoid infinite repetition
      else if (s.isEmpty) {
        val (s, b) = arr partitionInPlace (a ==)
        if (s.size > k) a
        else findKMedianInPlace(b, k - s.size)
      } else if (s.size < k) findKMedianInPlace(b, k - s.size)
      else findKMedianInPlace(s, k)
    }

    val pivot =
      (arr: ArrayView) => arr(Random.nextInt(arr.size))

    if (seq.length % 2 == 1)
      findKMedianInPlace(ArrayView(seq.toArray), (seq.size - 1) / 2)(pivot)
    else {
      val r1 = findKMedianInPlace(ArrayView(seq.toArray), seq.size / 2 - 1)(pivot)
      val r2 = findKMedianInPlace(ArrayView(seq.toArray), seq.size / 2)(pivot)
      Math.floorDiv(r1 + r2, 2)
    }
  }
}

object BaseGlobal {
  sealed trait Rounds
  case class RoundDown()     extends Rounds
  case class RoundUp()       extends Rounds
  case class RoundHalfDown() extends Rounds
  case class RoundHalfUp()   extends Rounds
  case class RoundHalfEven() extends Rounds
  case class RoundCeiling()  extends Rounds
  case class RoundFloor()    extends Rounds

  private case class ArrayView(arr: Array[Long], from: Int, until: Int) {
    def apply(n: Int): Long =
      if (from + n < until) arr(from + n)
      else throw new ArrayIndexOutOfBoundsException(n)

    def partitionInPlace(p: Long => Boolean): (ArrayView, ArrayView) = {
      var upper = until - 1
      var lower = from
      while (lower < upper) {
        while (lower < until && p(arr(lower))) lower += 1
        while (upper >= from && !p(arr(upper))) upper -= 1
        if (lower < upper) {
          val tmp = arr(lower); arr(lower) = arr(upper); arr(upper) = tmp
        }
      }
      (copy(until = lower), copy(from = lower))
    }

    def size: Int        = until - from
    def isEmpty: Boolean = size <= 0
  }

  private object ArrayView {
    def apply(arr: Array[Long]) =
      new ArrayView(arr, 0, arr.length)
  }

  case class DAppInfo(
      bytes: Array[Byte],
      dApp: DApp,
      maxComplexity: (String, Long),
      annotatedComplexities: Map[String, Long],
      verifierComplexity: Long,
      callableComplexities: Map[String, Long],
      stateCallsComplexities: Map[String, Long],
      userFunctionComplexities: Map[String, Long],
      globalVariableComplexities: Map[String, Long]
  )
}
