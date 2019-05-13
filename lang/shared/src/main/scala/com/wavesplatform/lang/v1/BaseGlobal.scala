package com.wavesplatform.lang.v1

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

  def base16Encode(input: Array[Byte]): Either[String, String]
  def base16Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

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
    Script.fromBase64String(compiledCode, checkComplexity = false).right.map{
      script =>
        val (scriptText, _) = Script.decompile(script)
        scriptText
    }
  }
}
