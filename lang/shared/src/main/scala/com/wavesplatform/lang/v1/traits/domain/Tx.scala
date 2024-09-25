package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.traits.domain.{Burn as BurnAction}

case class TransferItem(recipient: Recipient, amount: Long)

sealed trait Tx
sealed trait PseudoTx

object Tx {
  case class ScriptTransfer(
      assetId: Option[ByteStr],
      sender: Recipient.Address,
      senderPk: ByteStr,
      recipient: Recipient,
      amount: Long,
      timestamp: Long,
      id: ByteStr
  ) extends PseudoTx

  case class ReissuePseudoTx(
      reissue: Reissue,
      sender: Recipient.Address,
      senderPk: ByteStr,
      txId: ByteStr,
      timestamp: Long
  ) extends PseudoTx

  case class BurnPseudoTx(
      burn: BurnAction,
      sender: Recipient.Address,
      senderPk: ByteStr,
      txId: ByteStr,
      timestamp: Long
  ) extends PseudoTx

  case class SponsorFeePseudoTx(
      sponsorFee: SponsorFee,
      sender: Recipient.Address,
      senderPk: ByteStr,
      txId: ByteStr,
      timestamp: Long
  ) extends PseudoTx

  case class InvokePseudoTx(
      id: ByteStr,
      timestamp: Long,
      sender: Recipient.Address,
      senderPk: ByteStr,
      dApp: Recipient,
      feeAssetId: Option[ByteStr],
      funcName: Option[String],
      funcArgs: List[EVALUATED],
      payments: AttachedPayments
  ) extends PseudoTx

  case class Header(id: ByteStr, fee: Long, timestamp: Long, version: Long)
  case class Proven(h: Header, sender: Recipient.Address, bodyBytes: ByteStr, senderPk: ByteStr, proofs: IndexedSeq[ByteStr])
  case class TransferItem(recipient: Recipient, amount: Long)
  case class Pmt(asset: Option[ByteStr], amount: Long)

  case class Genesis(header: Header, amount: Long, recipient: Recipient) extends Tx

  case class Payment(p: Proven, amount: Long, recipient: Recipient) extends Tx

  case class Transfer(p: Proven, feeAssetId: Option[ByteStr], assetId: Option[ByteStr], amount: Long, recipient: Recipient, attachment: ByteStr)
      extends Tx

  case class Issue(p: Proven, quantity: Long, name: ByteStr, description: ByteStr, reissuable: Boolean, decimals: Long, script: Option[ByteStr])
      extends Tx
  case class ReIssue(p: Proven, quantity: Long, assetId: ByteStr, reissuable: Boolean) extends Tx
  case class Burn(p: Proven, quantity: Long, assetId: ByteStr)                         extends Tx

  case class CI(
      p: Proven,
      dAppAddressOrAlias: Recipient,
      payments: AttachedPayments,
      feeAssetId: Option[ByteStr],
      funcName: Option[String],
      funcArgs: List[EVALUATED]
  ) extends Tx

  case class InvokeExpression(p: Proven, expression: ByteStr, feeAssetId: Option[ByteStr]) extends Tx

  case class Lease(p: Proven, amount: Long, recipient: Recipient) extends Tx

  case class LeaseCancel(p: Proven, leaseId: ByteStr) extends Tx

  case class CreateAlias(p: Proven, alias: String) extends Tx

  case class SetScript(p: Proven, script: Option[ByteStr]) extends Tx

  case class SetAssetScript(p: Proven, assetId: ByteStr, script: Option[ByteStr]) extends Tx

  case class MassTransfer(
      p: Proven,
      assetId: Option[ByteStr],
      transferCount: Long,
      totalAmount: Long,
      transfers: IndexedSeq[TransferItem],
      attachment: ByteStr
  ) extends Tx

  case class Sponsorship(p: Proven, assetId: ByteStr, minSponsoredAssetFee: Option[Long]) extends Tx

  case class Exchange(p: Proven, amount: Long, price: Long, buyMatcherFee: Long, sellMatcherFee: Long, buyOrder: Ord, sellOrder: Ord) extends Tx
  case class Data(p: Proven, data: IndexedSeq[DataOp])                                                                                extends Tx
  case class UpdateAssetInfo(p: Proven, assetId: ByteStr, name: String, description: String)                                          extends Tx
}
