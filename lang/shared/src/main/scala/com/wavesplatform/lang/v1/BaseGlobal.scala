package com.wavesplatform.lang.v1

import com.wavesplatform.lang.ContentType
import com.wavesplatform.lang.StdLibVersion.StdLibVersion
import com.wavesplatform.lang.contract.{Contract, ContractSerDe}
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

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

  def keccak256(message: Array[Byte]): Array[Byte]
  def blake2b256(message: Array[Byte]): Array[Byte]
  def sha256(message: Array[Byte]): Array[Byte]

  def secureHash(a: Array[Byte]): Array[Byte] = keccak256(blake2b256(a))

  def checksum(arr: Array[Byte]): Array[Byte] = secureHash(arr).take(4)

  def serializeExpression(expr: EXPR, stdLibVersion: StdLibVersion): Array[Byte] = {
    val s = Array(stdLibVersion.toByte) ++ Serde.serialize(expr)
    s ++ checksum(s)
  }

  def serializeContract(c: Contract, stdLibVersion: StdLibVersion): Array[Byte] = {
    val s = Array(0: Byte, ContentType.Contract.toByte, stdLibVersion.toByte) ++ ContractSerDe.serialize(c)
    s ++ checksum(s)
  }

  def compileExpression(input: String,
                        context: CompilerContext,
                        restrictToLetBlockOnly: Boolean,
                        stdLibVersion: StdLibVersion): Either[String, (Array[Byte], Terms.EXPR)] = {
    for {
      ex <- ExpressionCompiler.compile(input, context)
      illegalBlockVersionUsage = restrictToLetBlockOnly && com.wavesplatform.lang.v1.compiler.сontainsBlockV2(ex)
      _ <- Either.cond(!illegalBlockVersionUsage, (), "UserFunctions are only enabled in STDLIB_VERSION >= 3")
      x = serializeExpression(ex, stdLibVersion)
    } yield (x, ex)
  }

  def compileContract(input: String, ctx: CompilerContext, stdLibVersion: StdLibVersion): Either[String, (Array[Byte], Contract)] = {
    ContractCompiler
      .compile(input, ctx)
      .map { ast =>
        val ser = serializeContract(ast, stdLibVersion)
        (ser, ast)
      }
  }
}
