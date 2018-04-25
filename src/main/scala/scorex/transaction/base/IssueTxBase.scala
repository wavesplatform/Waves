package scorex.transaction.base

import scorex.transaction.Transaction
import scorex.transaction.smart.script.Script

trait IssueTxBase { _: Transaction =>
  def name: Array[Byte]
  def description: Array[Byte]
  def quantity: Long
  def decimals: Byte
  def reissuable: Boolean
  def script: Option[Script]

}
