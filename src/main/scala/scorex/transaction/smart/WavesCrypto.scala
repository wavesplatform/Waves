package scorex.transaction.smart

import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.BaseGlobal
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{Blake2b256, Keccak256, Sha256}

object WavesCrypto extends BaseGlobal {
  override def base58Encode(input: Array[Byte]): String                 = Base58.encode(input)
  override def base58Decode(input: String): Either[String, Array[Byte]] = Base58.decode(input).toEither.left.map(_.toString)

  override def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean =
    crypto.verify(sig, message, pub)

  override def keccak256(message: Array[Byte]): Array[Byte] = Keccak256.hash(message)

  override def blake2b256(message: Array[Byte]): Array[Byte] = Blake2b256.hash(message)

  override def sha256(message: Array[Byte]): Array[Byte] = Sha256.hash(message)
}
