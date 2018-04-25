package scorex.transaction.base

import com.wavesplatform.state.ByteStr
import scorex.transaction.Transaction

trait CancelFeeSponsorshipTxBase { _: Transaction =>
  def assetId: ByteStr
}
