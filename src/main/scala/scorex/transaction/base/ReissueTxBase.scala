package scorex.transaction.base

import com.wavesplatform.state.ByteStr
import scorex.transaction.Transaction

trait ReissueTxBase { _:Transaction =>
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
}
