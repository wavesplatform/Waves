package com.wavesplatform

import java.lang.reflect.Constructor

import com.wavesplatform.account.{PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.*
import org.whispersystems.curve25519.OpportunisticCurve25519Provider

import scala.util.Try

package object crypto {
  // Constants
  val SignatureLength: Int   = Curve25519.SignatureLength // 64
  val KeyLength: Int         = Curve25519.KeyLength       // 32
  val DigestLength: Int      = 32
  val EthereumKeyLength: Int = 64

  // Additional provider
  private val provider: OpportunisticCurve25519Provider = {
    val constructor = classOf[OpportunisticCurve25519Provider].getDeclaredConstructors.head
      .asInstanceOf[Constructor[OpportunisticCurve25519Provider]]
    constructor.setAccessible(true)
    constructor.newInstance()
  }

  // Digests
  def fastHash(m: Array[Byte]): Array[Byte]   = Blake2b256.hash(m)
  def fastHash(s: String): Array[Byte]        = fastHash(s.utf8Bytes)
  def secureHash(m: Array[Byte]): Array[Byte] = Keccak256.hash(Blake2b256.hash(m))
  def secureHash(s: String): Array[Byte]      = secureHash(s.utf8Bytes)

  // Signatures
  def sign(account: PrivateKey, message: Array[Byte]): ByteStr =
    ByteStr(Curve25519.sign(account.arr, message))

  def signVRF(account: PrivateKey, message: Array[Byte]): ByteStr =
    ByteStr(provider.calculateVrfSignature(provider.getRandom(DigestLength), account.arr, message))

  def verify(signature: ByteStr, message: Array[Byte], publicKey: PublicKey, checkWeakPk: Boolean = false): Boolean = {
    (!checkWeakPk || !isWeakPublicKey(publicKey.arr)) && Curve25519.verify(signature.arr, message, publicKey.arr)
  }

  def verifyVRF(signature: ByteStr, message: Array[Byte], publicKey: PublicKey, checkWeakPk: Boolean = false): Either[ValidationError, ByteStr] = {
    for {
      _ <- Either.cond(!checkWeakPk || !isWeakPublicKey(publicKey.arr), (), GenericError("Could not verify VRF proof: weak public key is used"))
      result <- Try(ByteStr(provider.verifyVrfSignature(publicKey.arr, message, signature.arr))).toEither.left
        .map(_ => GenericError("Could not verify VRF proof"))
    } yield result
  }

  // see
  // https://github.com/jedisct1/libsodium/blob/ab4ab23d5744a8e060864a7cec1a7f9b059f9ddd/src/libsodium/crypto_scalarmult/curve25519/ref10/x25519_ref10.c#L17
  // https://boringssl.googlesource.com/boringssl/+/master/third_party/wycheproof_testvectors/x25519_test.json
  private[this] val BlacklistedKeys: Array[Array[Byte]] = Array(
    // 0 (order 4)
    Array(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00),
    // 1 (order 1)
    Array(0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00),
    // 325606250916557431795983626356110631294008115727848805560023387167927233504 (order 8)
    Array(0xe0, 0xeb, 0x7a, 0x7c, 0x3b, 0x41, 0xb8, 0xae, 0x16, 0x56, 0xe3, 0xfa, 0xf1, 0x9f, 0xc4, 0x6a, 0xda, 0x09, 0x8d, 0xeb, 0x9c, 0x32, 0xb1,
      0xfd, 0x86, 0x62, 0x05, 0x16, 0x5f, 0x49, 0xb8, 0x00),
    // 39382357235489614581723060781553021112529911719440698176882885853963445705823 (order 8)
    Array(0x5f, 0x9c, 0x95, 0xbc, 0xa3, 0x50, 0x8c, 0x24, 0xb1, 0xd0, 0xb1, 0x55, 0x9c, 0x83, 0xef, 0x5b, 0x04, 0x44, 0x5c, 0xc4, 0x58, 0x1c, 0x8e,
      0x86, 0xd8, 0x22, 0x4e, 0xdd, 0xd0, 0x9f, 0x11, 0x57),
    // p-1 (order 2)
    Array(0xec, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f),
    // p (=0, order 4)
    Array(0xed, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f),
    // p+1 (=1, order 1)
    Array(0xee, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x7f)
  ).map(_.map(_.toByte))

  def isWeakPublicKey(publicKey: Array[Byte]): Boolean =
    BlacklistedKeys.exists { wk =>
      publicKey.view.init.iterator.sameElements(wk.view.init) &&
      (publicKey.last == wk.last || (publicKey.last & 0xff) == wk.last + 0x80)
    }
}
