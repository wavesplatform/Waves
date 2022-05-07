package com.wavesplatform.lang.v1

import scala.annotation.tailrec
import scala.util.Random

import cats.syntax.either.*
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.meta.{FunctionSignatures, MetaMapper, ParsedMeta}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.serialization.{ContractSerDeV1, ContractSerDeV2}
import com.wavesplatform.lang.directives.values.{Account, Call, Expression, ScriptType, StdLibVersion, V1, V2, V6, DApp as DAppType}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.BaseGlobal.{ArrayView, DAppInfo}
import com.wavesplatform.lang.v1.compiler.{CompilationError, CompilerContext, ContractCompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.compiler.CompilationError.Generic
import com.wavesplatform.lang.v1.compiler.ScriptResultSource.CallableFunction
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.estimator.{ScriptEstimator, ScriptEstimatorV1}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding
import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding.*
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.parser.Expressions.Pos.AnyPos
import com.wavesplatform.lang.v1.serialization.{SerdeV1, SerdeV2}

/** This is a hack class for IDEA. The Global class is in JS/JVM modules. And IDEA can't find the Global class in the "shared" module, but it should!
  */
trait BaseGlobal {
  val MaxBase16Bytes: Int               = 8 * 1024
  val MaxBase16String: Int              = 32 * 1024
  val MaxBase58Bytes                    = 64
  val MaxBase58String                   = 100
  val MaxBase64Bytes: Int               = 32 * 1024
  val MaxBase64String: Int              = 44 * 1024
  val MaxLiteralLength: Int             = 12 * 1024
  val MaxAddressLength                  = 36
  val MaxByteStrSizeForVerifyFuncs: Int = 32 * 1024

  val LetBlockVersions: Set[StdLibVersion] = Set[StdLibVersion](V1, V2)

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
    val serialized = if (stdLibVersion < V6) {
      stdLibVersion.id.toByte +: SerdeV1.serialize(expr)
    } else {
      Array(stdLibVersion.id.toByte, Expression.id.toByte) ++ SerdeV2.serialize(expr)
    }

    serialized ++ checksum(serialized)
  }

  def serializeContract(c: DApp, stdLibVersion: StdLibVersion): Either[String, Array[Byte]] = {
    val serialized = if (stdLibVersion < V6) {
      ContractSerDeV1
        .serialize(c)
        .map(Array(0: Byte, DAppType.id.toByte, stdLibVersion.id.toByte) ++ _)
    } else {
      ContractSerDeV2
        .serialize(c)
        .map(Array(stdLibVersion.id.toByte, DAppType.id.toByte) ++ _)
    }

    serialized.map(r => r ++ checksum(r))
  }

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
      costs = utils.functionCosts(stdLibVersion, DAppType)
      complexity <- if (compErrorList.isEmpty) estimator(vars, costs, compExpr) else Either.right(0L)
    } yield (bytes, complexity, exprScript, compErrorList))
      .recover { case e =>
        (Array.empty, 0, Expressions.SCRIPT(AnyPos, Expressions.INVALID(AnyPos, "Unknown error.")), List(Generic(0, 0, e)))
      }
  }

  def parseAndCompileContract(
      input: String,
      ctx: CompilerContext,
      stdLibVersion: StdLibVersion,
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean
  ): Either[String, (Array[Byte], (Long, Map[String, Long]), Expressions.DAPP, Iterable[CompilationError])] = {
    (for {
      compRes <- ContractCompiler.compileWithParseResult(input, ctx, stdLibVersion, needCompaction, removeUnusedCode)
      (compDAppOpt, exprDApp, compErrorList) = compRes
      complexityWithMap <-
        if (compDAppOpt.nonEmpty && compErrorList.isEmpty)
          ContractScript.estimateComplexity(stdLibVersion, compDAppOpt.get, estimator, fixEstimateOfVerifier = true)
        else Right((0L, Map.empty[String, Long]))
      bytes <- if (compDAppOpt.nonEmpty && compErrorList.isEmpty) serializeContract(compDAppOpt.get, stdLibVersion) else Right(Array.empty[Byte])
    } yield (bytes, complexityWithMap, exprDApp, compErrorList))
      .recover { case e =>
        (Array.empty, (0, Map.empty), Expressions.DAPP(AnyPos, List.empty, List.empty), List(Generic(0, 0, e)))
      }
  }

  val compileExpression: (String, CompilerContext, StdLibVersion, ScriptType, ScriptEstimator) => Either[String, (Array[Byte], EXPR, Long)] =
    compile(_, _, _, _, _, ExpressionCompiler.compileBoolean)

  val compileFreeCall: (String, CompilerContext, StdLibVersion, ScriptType, ScriptEstimator) => Either[String, (Array[Byte], EXPR, Long)] =
    (input, ctx, version, scriptType, estimator) =>
      compile(input, ctx, version, scriptType, estimator, ContractCompiler.compileFreeCall(_, _, version))

  val compileDecls: (String, CompilerContext, StdLibVersion, ScriptType, ScriptEstimator) => Either[String, (Array[Byte], EXPR, Long)] =
    compile(_, _, _, _, _, ExpressionCompiler.compileDecls)

  private def compile(
      input: String,
      context: CompilerContext,
      version: StdLibVersion,
      scriptType: ScriptType,
      estimator: ScriptEstimator,
      compiler: (String, CompilerContext) => Either[String, EXPR]
  ): Either[String, (Array[Byte], EXPR, Long)] = {
    val isFreeCall = scriptType == Call
    for {
      expr <- if (isFreeCall) ContractCompiler.compileFreeCall(input, context, version) else compiler(input, context)
      bytes = serializeExpression(expr, version)
      _          <- ExprScript.validateBytes(bytes, isFreeCall)
      complexity <- ExprScript.estimate(expr, version, isFreeCall, estimator, scriptType == Account)
    } yield (bytes, expr, complexity)
  }

  def checkExpr(
      expr: EXPR,
      complexity: Long,
      version: StdLibVersion,
      scriptType: ScriptType,
      estimator: ScriptEstimator
  ): Either[String, Unit] =
    for {
      _ <-
        if (estimator == ScriptEstimatorV2)
          ExprScript.estimate(expr, version, scriptType == Call, ScriptEstimatorV1, scriptType == Account)
        else
          Right(())
      _ <- ExprScript.checkComplexity(version, complexity, scriptType == Account, scriptType == Call)
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
      estimator: ScriptEstimator,
      needCompaction: Boolean,
      removeUnusedCode: Boolean
  ): Either[String, DAppInfo] =
    for {
      dApp                       <- ContractCompiler.compile(input, ctx, stdLibVersion, CallableFunction, needCompaction, removeUnusedCode)
      bytes                      <- serializeContract(dApp, stdLibVersion)
      _                          <- ContractScript.validateBytes(bytes)
      userFunctionComplexities   <- ContractScript.estimateUserFunctions(stdLibVersion, dApp, estimator)
      globalVariableComplexities <- ContractScript.estimateGlobalVariables(stdLibVersion, dApp, estimator)
      (maxComplexity, annotatedComplexities) <- ContractScript.estimateComplexityExact(stdLibVersion, dApp, estimator, fixEstimateOfVerifier = true)
      _ <- ContractScript.checkComplexity(stdLibVersion, dApp, maxComplexity, annotatedComplexities, useReducedVerifierLimit = true)
      (verifierComplexity, callableComplexities) = dApp.verifierFuncOpt.fold(
        (0L, annotatedComplexities)
      )(v => (annotatedComplexities(v.u.name), annotatedComplexities - v.u.name))
    } yield DAppInfo(
      bytes,
      dApp,
      maxComplexity,
      annotatedComplexities,
      verifierComplexity,
      callableComplexities,
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
      _ <-
        if (estimator == ScriptEstimatorV2)
          ContractScript.estimateComplexity(version, dApp, ScriptEstimatorV1, fixEstimateOfVerifier = true)
        else
          Right(())
      _ <- ContractScript.checkComplexity(version, dApp, maxComplexity, complexities, useReducedVerifierLimit = true)
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
      meta.callableFuncTypes.fold(List.empty[(String, List[(String, FINAL)])])(types =>
        (types zip dApp.callableFuncs)
          .map { case (argTypes, func) =>
            func.u.name -> (func.u.args zip argTypes)
          }
      )
    FunctionSignatures(meta.version, argTypesWithFuncName)
  }

  def merkleVerify(rootBytes: Array[Byte], proofBytes: Array[Byte], valueBytes: Array[Byte]): Boolean

  // Math functions

  def pow(b: Long, bp: Int, e: Long, ep: Int, rp: Int, round: Rounding, useNewPrecision: Boolean): Either[String, Long]
  def log(b: Long, bp: Long, e: Long, ep: Long, rp: Long, round: Rounding): Either[String, Long]
  def powBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding, useNewPrecision: Boolean): Either[String, BigInt]
  def logBigInt(b: BigInt, bp: Long, e: BigInt, ep: Long, rp: Long, round: Rounding): Either[String, BigInt]

  def divide(a: BigInt, b: BigInt, rounding: Rounding): Either[String, BigInt] = {
    val sign                  = a.sign * b.sign
    val (division, remainder) = a.abs /% b.abs
    rounding match {
      case Down => Right(division * sign)
      case Up   => Right((division + remainder.sign) * sign)
      case HalfUp =>
        val x = b.abs - remainder * 2
        if (x <= 0) {
          Right((division + 1) * sign)
        } else {
          Right(division * sign)
        }
      case HalfDown =>
        val x = b.abs - remainder * 2
        if (x < 0) {
          Right((division + 1) * sign)
        } else {
          Right(division * sign)
        }
      case HalfEven =>
        val x = b.abs - remainder * 2
        if (x < 0) {
          Right((division + 1) * sign)
        } else if (x > 0) {
          Right(division * sign)
        } else {
          Right((division + division % 2) * sign)
        }
      case Ceiling =>
        Right((if (sign > 0) {
                 division + remainder.sign
               } else {
                 division
               }) * sign)
      case Floor =>
        Right((if (sign < 0) {
                 division + remainder.sign
               } else {
                 division
               }) * sign)
      case _ =>
        Left(s"unsupported rounding $rounding")
    }
  }

  def groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean

  def bn256Groth16Verify(verifyingKey: Array[Byte], proof: Array[Byte], inputs: Array[Byte]): Boolean

  def ecrecover(messageHash: Array[Byte], signature: Array[Byte]): Array[Byte]

  def median[@specialized T](seq: Array[T])(implicit num: Integral[T]): T = {
    import num.*
    @tailrec
    def findKMedianInPlace(arr: ArrayView[T], k: Int)(implicit choosePivot: ArrayView[T] => T): T = {
      val a      = choosePivot(arr)
      val (s, b) = arr partitionInPlace (a > _)
      if (s.size == k) a
      // The following test is used to avoid infinite repetition
      else if (s.isEmpty) {
        val (s, b) = arr partitionInPlace (a == _)
        if (s.size > k) a
        else findKMedianInPlace(b, k - s.size)
      } else if (s.size < k) findKMedianInPlace(b, k - s.size)
      else findKMedianInPlace(s, k)
    }

    val pivot =
      (arr: ArrayView[T]) => arr(Random.nextInt(arr.size))

    if (seq.length % 2 == 1)
      findKMedianInPlace(ArrayView[T](seq), (seq.size - 1) / 2)(pivot)
    else {
      val r1 = findKMedianInPlace(ArrayView[T](seq), seq.size / 2 - 1)(pivot)
      val r2 = findKMedianInPlace(ArrayView[T](seq), seq.size / 2)(pivot)
      // save Math.floorDiv(r1 + r2, 2) semantic and avoid overflow
      if (num.sign(r1) == num.sign(r2)) {
        if (r1 < r2) {
          num.abs(r2 - r1) / num.fromInt(2) + r1
        } else {
          num.abs(r1 - r2) / num.fromInt(2) + r2
        }
      } else {
        val d   = r1 + r2
        val two = num.fromInt(2)
        if (d >= num.zero || d % two == 0) { // handle Long.MinValue for T=Long
          d / two
        } else {
          (d - num.one) / two
        }
      }
    }
  }

  def isIllFormed(s: String): Boolean
}

object BaseGlobal {

  private case class ArrayView[@specialized T](arr: Array[T], from: Int, until: Int)(implicit num: Integral[T]) {
    def apply(n: Int): T =
      if (from + n < until) arr(from + n)
      else throw new ArrayIndexOutOfBoundsException(n)

    def partitionInPlace(p: T => Boolean): (ArrayView[T], ArrayView[T]) = {
      var upper = until - 1
      var lower = from
      while (lower < upper) {
        while (lower < until && p(arr(lower))) lower += 1
        while (upper >= from && !p(arr(upper))) upper -= 1
        if (lower < upper) { val tmp = arr(lower); arr(lower) = arr(upper); arr(upper) = tmp }
      }
      (copy(until = lower), copy(from = lower))
    }

    def size: Int        = until - from
    def isEmpty: Boolean = size <= 0
  }

  private object ArrayView {
    def apply[@specialized T](arr: Array[T])(implicit num: Integral[T]) =
      new ArrayView(arr, 0, arr.length)
  }

  case class DAppInfo(
      bytes: Array[Byte],
      dApp: DApp,
      maxComplexity: (String, Long),
      annotatedComplexities: Map[String, Long],
      verifierComplexity: Long,
      callableComplexities: Map[String, Long],
      userFunctionComplexities: Map[String, Long],
      globalVariableComplexities: Map[String, Long]
  )
}
