package scorex.transaction.assets

import com.wavesplatform.state2.ByteArray
import scorex.transaction.SignedTransaction

/*
  Issue or Reissue of Asset
 */
trait AssetIssuance extends SignedTransaction {
  val assetId: ByteArray
  val reissuable: Boolean
  val quantity: Long
}
