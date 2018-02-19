package scorex.waves.crypto

import scorex.crypto.hash._

object HashChain extends CryptographicHash32 {
  override def hash(in: Message): Digest32 = {
    Keccak256.hash(Blake2b256.hash(in))
  }
}
