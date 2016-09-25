package scorex

import scorex.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = Array[Byte]
  val AssetIdLength = FastCryptographicHash.DigestSize

}
