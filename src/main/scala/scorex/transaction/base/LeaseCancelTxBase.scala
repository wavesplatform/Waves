package scorex.transaction.base

import com.wavesplatform.state.ByteStr

trait LeaseCancelTxBase extends TxBase {
  def leaseId: ByteStr
}
