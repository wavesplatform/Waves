package scorex.transaction.base

import com.wavesplatform.state.DataEntry
import scorex.transaction.Transaction

trait DataTxBase { _: Transaction =>
  def data: List[DataEntry[_]]
}
