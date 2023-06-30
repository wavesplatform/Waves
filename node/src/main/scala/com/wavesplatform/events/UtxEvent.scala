package com.wavesplatform.events

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.transaction.Transaction

sealed trait UtxEvent
object UtxEvent {
  final case class TxAdded(tx: Transaction, snapshot: StateSnapshot) extends UtxEvent {
    override def toString: String = s"TxAdded(${tx.id()})"
  }
  final case class TxRemoved(tx: Transaction, reason: Option[ValidationError]) extends UtxEvent {
    override def toString: String = s"TxRemoved(${tx.id()})"
  }
}
