package scorex

import com.wavesplatform.utils.base58Length
import scorex.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = Array[Byte]
  val AssetIdLength = FastCryptographicHash.DigestSize
  val AssetIdStringLength = base58Length(AssetIdLength)

}
