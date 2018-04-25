package scorex.transaction.base

import com.wavesplatform.state.DataEntry

trait DataTxBase extends TxBase {
  def data: List[DataEntry[_]]
}
