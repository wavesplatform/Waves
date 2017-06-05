package scorex

import com.wavesplatform.utils.base58Length
import scorex.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = com.wavesplatform.state2.ByteStr
  val AssetIdLength = FastCryptographicHash.DigestSize
  val AssetIdStringLength = base58Length(AssetIdLength)

}
