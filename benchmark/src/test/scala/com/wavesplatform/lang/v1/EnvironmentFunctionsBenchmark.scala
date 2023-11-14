package com.wavesplatform.lang.v1

import java.nio.charset.StandardCharsets
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import cats.Id
import cats.syntax.bifunctor.*
import com.wavesplatform.account
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.Curve25519
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V4}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark.*
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_STRING, EVALUATED, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Functions, WavesContext}
import com.wavesplatform.lang.v1.traits.*
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.{Common, Global, ValidationError}
import com.wavesplatform.wallet.Wallet
import monix.eval.Coeval
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@OutputTimeUnit(TimeUnit.MICROSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10, time = 1)
@Measurement(iterations = 10, time = 1)
class EnvironmentFunctionsBenchmark {

  @Benchmark
  def random_bytes_500_test(): Array[Byte] = randomBytes(DataBytesLength)

  @Benchmark
  def base58_decode_full_test(): Either[String, Array[Byte]] =
    Global.base58Decode(Global.base58Encode(randomBytes(Base58BytesLength)).explicitGet())

  @Benchmark
  def base58_encode_test(): String = hashTest(Base58BytesLength, Global.base58Encode(_).explicitGet())

  @Benchmark
  def base58_26_encode_test(): String = hashTest(26, Global.base58Encode(_).explicitGet()) // for addressFromString_full_test

  @Benchmark
  def base16_decode_test(): Array[Byte] = Global.base16Decode(string32Kb, checkLength = true).explicitGet()

  @Benchmark
  def base16_encode_test(): String = Global.base16Encode(bytes8Kb, checkLength = true).explicitGet()

  @Benchmark
  def sha256_test(): Array[Byte] = hashTest(Global.sha256)

  @Benchmark
  def keccak256_test(): Array[Byte] = hashTest(Global.keccak256)

  @Benchmark
  def blake2b256_test(): Array[Byte] = hashTest(Global.blake2b256)

  @Benchmark
  def secureHash_test(): Array[Byte] = hashTest(Global.secureHash)

  @Benchmark
  def curve25519_generateKeypair_test(): (Array[Byte], Array[Byte]) = curve25519.generateKeypair

  @Benchmark
  def curve25519_sign_full_test(): Array[Byte] = {
    val (privateKey, _) = curve25519.generateKeypair
    curve25519.sign(privateKey, randomBytes(DataBytesLength))
  }

  @Benchmark
  def curve25519_full_test(): Boolean = {
    val (privateKey, publicKey) = curve25519.generateKeypair
    val message                 = randomBytes(DataBytesLength)
    val signature               = curve25519.sign(privateKey, message)
    Curve25519.verify(signature, message, publicKey)
  }

  @Benchmark
  def addressFromString(st: AddressFromString, bh: Blackhole): Unit = {
    val i = Random.nextInt(100)
    bh.consume(eval(st.ctx, st.expr(i), V4))
  }
}

object EnvironmentFunctionsBenchmark {

  val ChainId: Byte     = 'P'
  val Base58BytesLength = Global.MaxBase58Bytes
  val DataBytesLength   = 512
  val SeedBytesLength   = 128

  val environment: Environment[Id] = new Environment[Id] {
    override def height: Long                                                                                    = 1
    override def chainId: Byte                                                                                   = ChainId
    override def inputEntity: Environment.InputEntity                                                            = ???
    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer]                                   = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def hasData(recipient: Recipient): Boolean                                                          = false
    override def resolveAlias(alias: String): Either[String, Recipient.Address]                                  = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
    override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                                         = ???
    override def lastBlockOpt(): Option[BlockInfo]                                                               = ???
    override def blockInfoByHeight(height: Int): Option[BlockInfo]                                               = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
    override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails]    = ???
    override def tthis: Environment.Tthis                                                                        = ???
    override def multiPaymentAllowed: Boolean                                                                    = ???
    override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer]                               = ???
    override def txId: ByteStr                                                                                   = ByteStr(new Array[Byte](64))
    override def addressFromString(addressStr: String): Either[String, Recipient.Address] =
      account.Address
        .fromString(addressStr)
        .bimap(
          _.toString,
          address => Address(ByteStr(address.bytes))
        )

    override def addressFromPublicKey(publicKey: ByteStr): Either[String, Address] =
      Right(Address(ByteStr(PublicKey(publicKey).toAddress.bytes)))

    override def accountScript(addressOrAlias: Recipient): Option[Script] = ???

    override def calculateDelay(hitSource: ByteStr, baseTarget: Long, generator: ByteStr, balance: Long): Long = ???

    def callScript(
        dApp: Address,
        func: String,
        args: List[EVALUATED],
        payments: Seq[(Option[Array[Byte]], Long)],
        availableComplexity: Int,
        reentrant: Boolean
    ): Coeval[Id[(Either[ValidationError, (EVALUATED, Log[Id])], Int)]] = ???
  }

  val environmentFunctions = new EnvironmentFunctions(environment)

  val string32Kb: String = "FEDCBA9876543210" * (32 * 1024 / 16)

  val bytes8Kb: Array[Byte] = ("FEDCBA9876543210" * (8 * 1024 / 16)).getBytes(StandardCharsets.UTF_8)

  def randomBytes(length: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

  def randomAddress: ByteStr = ByteStr(Common.addressFromPublicKey(ChainId, randomBytes(Curve25519.KeyLength)))

  def hashTest[T](f: Array[Byte] => T): T           = f(randomBytes(DataBytesLength))
  def hashTest[T](len: Int, f: Array[Byte] => T): T = f(randomBytes(len))

  object curve25519 {
    def generateKeypair                                                  = Curve25519.createKeyPair(randomBytes(SeedBytesLength))
    def sign(privateKey: Array[Byte], message: Array[Byte]): Array[Byte] = Curve25519.sign(privateKey, message)
  }
}

@State(Scope.Benchmark)
class AddressFromString {
  val ctx: EvaluationContext[Environment, Id] =
    WavesContext
      .build(Global, DirectiveSet(V4, Account, DApp).explicitGet(), true)
      .evaluationContext(environment)

  val expr: Array[EXPR] =
    (1 to 100).map { _ =>
      val address =
        Wallet
          .generateNewAccount(Random.nextBytes(8), 1)
          .publicKey
          .toAddress(ChainId)
          .toString

      FUNCTION_CALL(
        Functions.addressFromStringV4.header,
        List(CONST_STRING(address).explicitGet())
      )
    }.toArray
}
