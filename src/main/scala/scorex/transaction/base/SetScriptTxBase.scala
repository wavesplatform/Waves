package scorex.transaction.base

import scorex.transaction.Transaction
import scorex.transaction.smart.script.Script

trait SetScriptTxBase { _: Transaction =>
  def script: Option[Script]
}
