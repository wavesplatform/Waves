package scorex.transaction.base

import com.wavesplatform.state.ByteStr
import scorex.transaction.Transaction

trait LeaseCancelTxBase { _: Transaction =>
  def leaseId: ByteStr
}
