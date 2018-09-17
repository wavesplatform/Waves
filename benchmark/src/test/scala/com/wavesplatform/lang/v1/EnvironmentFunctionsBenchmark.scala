package com.wavesplatform.lang.v1

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.lang.v1.EnvironmentFunctionsBenchmark._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.EnvironmentFunctions
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.state.EitherExt2
import org.openjdk.jmh.annotations._
import scodec.bits.ByteVector
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
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
  def sha256_test(): Array[Byte] = hashTest(Global.sha256)

  @Benchmark
  def keccak256_test(): Array[Byte] = hashTest(Global.keccak256)

  @Benchmark
  def blake2b256_test(): Array[Byte] = hashTest(Global.blake2b256)

  @Benchmark
  def secureHash_test(): Array[Byte] = hashTest(Global.secureHash)

  @Benchmark
  def curve25519_generateKeypair_test(): (PrivateKey, PublicKey) = curve25519.generateKeypair

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
    Curve25519.verify(Signature @@ signature, message, publicKey)
  }

  @Benchmark
  def addressFromPublicKey_test(): ByteVector = randomAddress

}

object EnvironmentFunctionsBenchmark {

  val NetworkByte: Byte = 'P'
  val Base58BytesLength = Global.MaxBase58Bytes
  val DataBytesLength   = 512
  val SeedBytesLength   = 128

  private val defaultEnvironment: Environment = new Environment {
    override def block: Blk                                                                                      = Blk(0, 1, ByteVector.empty)
    override def networkByte: Byte                                                                               = NetworkByte
    override def transaction: Tx                                                                                 = ???
    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def resolveAlias(alias: String): Either[String, Recipient.Address]                                  = ???
    override def transactionHeightById(id: Array[Byte]): Option[Int]                                             = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
  }

  val environmentFunctions = new EnvironmentFunctions(defaultEnvironment)

  def randomBytes(length: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](length)(0)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

  def randomAddress: ByteVector = ByteVector(Common.addressFromPublicKey(NetworkByte, randomBytes(Curve25519.KeyLength)))

  def hashTest[T](f: Array[Byte] => T): T           = f(randomBytes(DataBytesLength))
  def hashTest[T](len: Int, f: Array[Byte] => T): T = f(randomBytes(len))

  object curve25519 {
    def generateKeypair: (PrivateKey, PublicKey)                        = Curve25519.createKeyPair(randomBytes(SeedBytesLength))
    def sign(privateKey: PrivateKey, message: Array[Byte]): Array[Byte] = Curve25519.sign(privateKey, message)
  }

}
