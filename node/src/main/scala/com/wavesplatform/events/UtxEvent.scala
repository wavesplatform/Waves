package com.wavesplatform.events

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Diff
import com.wavesplatform.transaction.Transaction

sealed trait UtxEvent
object UtxEvent {
  final case class TxAdded(tx: Transaction, diff: Diff) extends UtxEvent
  final case class TxRemoved(tx: Transaction, reason: Option[ValidationError]) extends UtxEvent
}
