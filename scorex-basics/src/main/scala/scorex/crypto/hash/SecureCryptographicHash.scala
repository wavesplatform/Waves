package scorex.crypto.hash

import com.typesafe.config.ConfigFactory
import scorex.crypto.hash.CryptographicHash._
import scorex.utils._

import scala.util.Try


/**
 * Hash function for cases, where security is more important, then speed
 */
object SecureCryptographicHash extends CryptographicHash {

  private val hf: CryptographicHash = Try(ConfigFactory.load().getConfig("scorex").getString("secureHash"))
    .flatMap(s => objectFromString[CryptographicHash](s)).getOrElse(ScorexHashChain)

  override val DigestSize: Int = hf.DigestSize

  override def hash(in: Message): Digest = hf.hash(in)
}
