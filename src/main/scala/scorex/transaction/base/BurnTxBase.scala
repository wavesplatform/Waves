package scorex.transaction.base

import com.wavesplatform.state.ByteStr
import scorex.transaction.Transaction

trait BurnTxBase { _: Transaction =>
  def assetId: ByteStr
  def amount: Long
}
