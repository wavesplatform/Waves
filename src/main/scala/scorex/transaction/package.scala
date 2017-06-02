package scorex

import com.wavesplatform.state2.ByteArray
import com.wavesplatform.utils.base58Length
import scorex.crypto.hash.FastCryptographicHash

package object transaction {

  type AssetId = ByteArray
  val AssetIdLength = FastCryptographicHash.DigestSize
  val AssetIdStringLength = base58Length(AssetIdLength)

}
