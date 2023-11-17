package com.wavesplatform.state
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.lease.LeaseCancelTransaction

import scala.util.Try

case class LeaseSnapshot(sender: PublicKey, recipient: AddressOrAlias, amount: Long, status: LeaseDetails.Status) {
  val isActive: Boolean = status == LeaseDetails.Status.Active

  def toDetails(blockchain: Blockchain, txOpt: Option[Transaction], innerDetails: => Option[LeaseDetails]): LeaseDetails = {
    def height(id: ByteStr) = blockchain.transactionMeta(id).map(_.height).getOrElse(blockchain.height)
    val (sourceId, sourceHeight) = txOpt match {
      case Some(c: LeaseCancelTransaction)              => (c.leaseId, height(c.leaseId))
      case Some(i) if isActive                          => (i.id(), blockchain.height) // produced by lease or invoke (including eth)
      case Some(i) if !isActive && innerDetails.isEmpty => (i.id(), blockchain.height) // produced and cancelled by the same invoke
      case _ =>
        def id = innerDetails.get.sourceId // cancelled by invoke and produced by other transaction from the state
        Try((id, height(id))).getOrElse((ByteStr.empty, 0))
    }
    LeaseDetails(sender, recipient, amount, status, sourceId, sourceHeight)
  }
}

object LeaseSnapshot {
  def fromDetails(details: LeaseDetails): LeaseSnapshot =
    LeaseSnapshot(details.sender, details.recipient, details.amount, details.status)
}
