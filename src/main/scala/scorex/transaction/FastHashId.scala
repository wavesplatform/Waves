package scorex.transaction

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.crypto.hash.FastCryptographicHash

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(FastCryptographicHash(bodyBytes())))
}
