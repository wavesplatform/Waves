package scorex.transaction.base

import com.wavesplatform.state.ByteStr
import scorex.transaction.Transaction

trait SponsorFeeTxBase { _: Transaction =>
  def assetId: ByteStr
  def minFee: Long
}
