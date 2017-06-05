package scorex.transaction.assets

import com.wavesplatform.state2.ByteStr
import scorex.transaction.SignedTransaction

/*
  Issue or Reissue of Asset
 */
trait AssetIssuance extends SignedTransaction {
  val assetId: ByteStr
  val reissuable: Boolean
  val quantity: Long
}
