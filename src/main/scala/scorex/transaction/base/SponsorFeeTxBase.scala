package scorex.transaction.base

import com.wavesplatform.state.ByteStr

trait SponsorFeeTxBase extends TxBase {
  def assetId: ByteStr
  def minFee: Long
}
