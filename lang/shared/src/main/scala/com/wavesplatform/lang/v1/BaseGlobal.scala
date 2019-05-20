package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.evaluator.ctx.impl.crypto.RSA.DigestAlgorithm
import com.wavesplatform.lang.ValidationError.ScriptParseError
import com.wavesplatform.lang.contract.{ContractSerDe, DApp}
import com.wavesplatform.lang.directives.values.{Expression, StdLibVersion, DApp => DAppType}
import com.wavesplatform.lang.script.{ContractScript, Script}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ContractCompiler, ExpressionCompiler, Terms}

/**
  * This is a hack class for IDEA. The Global class is in JS/JVM modules.
  * And IDEA can't find the Global class in the "shared" module, but it must!
  */
trait BaseGlobal {
  val MaxBase58Bytes   = 64
  val MaxBase58String  = 100
  val MaxBase64Bytes   = 32 * 1024
  val MaxBase64String  = 44 * 1024
  val MaxLiteralLength = 12 * 1024
  val MaxAddressLength = 36

  def base58Encode(input: Array[Byte]): Either[String, String]
  def base58Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def base64Encode(input: Array[Byte]): Either[String, String]
  def base64Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  val hex : Array[Char] = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
  def base16Encode(input: Array[Byte]): Either[String, String] = {
    val output = new StringBuilder(input.size * 2)
    for (b <- input) {
       output.append(hex((b >> 4) & 0xf))
       output.append(hex(b & 0xf))
    }
    Right(output.result)
  }

  def base16Dig(c: Char): Either[String, Byte] = {
    if ('0' <= c && c <= '9') {
      Right((c - '0').toByte)
    } else if ('a' <= c && c <= 'f') {
      Right((10 + (c - 'a')).toByte)
    } else if ('A' <= c && c <= 'F') {
      Right((10 + (c - 'A')).toByte)
    } else {
      Left(s"$c isn't base16/hex digit")
    }
  }

  def base16Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]] = {
    val size = input.size
    if(size % 2 == 1) {
      Left("Need internal bytes number")
    } else {
      val bytes = new Array[Byte](size / 2)
      for( i <- 0 to size/2-1 ) {
        (base16Dig(input(i*2)), base16Dig(input(i*2 + 1))) match {
          case (Right(h), Right(l)) => bytes(i) = ((16:Byte)*h + l).toByte
          case (Left(e),_) => return Left(e)
          case (_,Left(e)) => return Left(e)
        }
      }
      Right(bytes)
    }
  }


  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

  def rsaVerify(alg: DigestAlgorithm, message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

  def keccak256(message: Array[Byte]): Array[Byte]
  def blake2b256(message: Array[Byte]): Array[Byte]
  def sha256(message: Array[Byte]): Array[Byte]

  def secureHash(a: Array[Byte]): Array[Byte] = keccak256(blake2b256(a))

  def checksum(arr: Array[Byte]): Array[Byte] = secureHash(arr).take(4)

  def serializeExpression(expr: EXPR, stdLibVersion: StdLibVersion): Array[Byte] = {
    val s = Array(stdLibVersion.id.toByte) ++ Serde.serialize(expr)
    s ++ checksum(s)
  }

  def serializeContract(c: DApp, stdLibVersion: StdLibVersion): Array[Byte] = {
    val s = Array(0: Byte, DAppType.id.toByte, stdLibVersion.id.toByte) ++ ContractSerDe.serialize(c)
    s ++ checksum(s)
  }

  def compileExpression(input: String,
                        context: CompilerContext,
                        restrictToLetBlockOnly: Boolean,
                        stdLibVersion: StdLibVersion): Either[String, (Array[Byte], Terms.EXPR, Long)] =
    for {
      ex <- ExpressionCompiler.compile(input, context)
      illegalBlockVersionUsage = restrictToLetBlockOnly && com.wavesplatform.lang.v1.compiler.сontainsBlockV2(ex)
      _ <- Either.cond(!illegalBlockVersionUsage, (), "UserFunctions are only enabled in STDLIB_VERSION >= 3")
      x = serializeExpression(ex, stdLibVersion)

      vars  = utils.varNames(stdLibVersion, Expression)
      costs = utils.functionCosts(stdLibVersion)
      complexity <- ScriptEstimator(vars, costs, ex)
    } yield (x, ex, complexity)

  def compileContract(input: String, ctx: CompilerContext, stdLibVersion: StdLibVersion): Either[String, (Array[Byte], DApp, Long)] =
    for {
      dapp       <- ContractCompiler.compile(input, ctx)
      complexity <- ContractScript.estimateComplexity(stdLibVersion, dapp)
    } yield (serializeContract(dapp, stdLibVersion), dapp, complexity._2)

  def decompile(compiledCode: String): Either[ScriptParseError, String] = {
    Script.fromBase64String(compiledCode, checkComplexity = false).right.map { script =>
      val (scriptText, _) = Script.decompile(script)
      scriptText
    }
  }
}
