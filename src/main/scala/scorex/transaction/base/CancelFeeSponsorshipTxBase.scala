package scorex.transaction.base

import com.wavesplatform.state.ByteStr

trait CancelFeeSponsorshipTxBase extends TxBase {
  def assetId: ByteStr
}
