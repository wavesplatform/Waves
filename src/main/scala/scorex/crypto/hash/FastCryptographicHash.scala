package scorex.crypto.hash

import com.typesafe.config.ConfigFactory
import scorex.crypto.hash.CryptographicHash._
import scorex.utils._

import scala.util.Try

/**
 * Fast and secure hash function
 */
object FastCryptographicHash extends CryptographicHash {

  private val hf: CryptographicHash = Try(ConfigFactory.load().getConfig("scorex").getString("fastHash"))
    .flatMap(s => objectFromString[CryptographicHash](s)).getOrElse(Blake256)

  override val DigestSize: Int = hf.DigestSize

  override def hash(in: Message): Digest = hf.hash(in)

}
