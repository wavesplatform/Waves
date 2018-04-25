package scorex.transaction.base

import scorex.transaction.smart.script.Script

trait SetScriptTxBase extends TxBase {
  def script: Option[Script]
}
