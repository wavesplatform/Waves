package com.wavesplatform.lang

import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.wavesplatform.lang.GlobalFunctionsBenchmark._
import com.wavesplatform.lang.traits.{DataType, Environment, Transaction}
import org.openjdk.jmh.annotations._
import scodec.bits.ByteVector
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.{Curve25519, PrivateKey, PublicKey, Signature}

@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(4)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class GlobalFunctionsBenchmark {

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
  def curve25519_sign_test(): Array[Byte] = {
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

  @Benchmark
  def addressFromString_full_test(): Either[String, Option[ByteVector]] = environmentFunctions.addressFromString(Base58.encode(randomAddress.toArray))

}

object GlobalFunctionsBenchmark {

  val NetworkByte: Byte = 'P'
  val DataBytesLength   = 512
  val SeedBytesLength   = 128

  private val defaultEnvironment: Environment = new Environment {
    override def height: Int                                                                   = 1
    override def networkByte: Byte                                                             = NetworkByte
    override def transaction: Transaction                                                      = ???
    override def transactionById(id: Array[Byte]): Option[Transaction]                         = ???
    override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = ???
    override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]]      = ???
  }

  val environmentFunctions = new EnvironmentFunctions(defaultEnvironment)

  def randomBytes(length: Int): Array[Byte] = {
    val bytes = Array.fill[Byte](DataBytesLength)(0)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

  def randomAddress: ByteVector = environmentFunctions.addressFromPublicKey(ByteVector(randomBytes(Curve25519.KeyLength)))

  def hashTest(f: Array[Byte] => Array[Byte]): Array[Byte] = f(randomBytes(DataBytesLength))

  object curve25519 {
    def generateKeypair: (PrivateKey, PublicKey)                        = Curve25519.createKeyPair(randomBytes(SeedBytesLength))
    def sign(privateKey: PrivateKey, message: Array[Byte]): Array[Byte] = Curve25519.sign(privateKey, message)
  }

}
