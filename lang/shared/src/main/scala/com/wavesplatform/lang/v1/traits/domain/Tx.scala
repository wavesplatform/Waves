package com.wavesplatform.lang.v1.traits.domain

import scodec.bits.ByteVector

case class TransferItem(recipient: Recipient, amount: Long)

trait Tx

object Tx {

  case class Header(id: ByteVector, fee: Long, timestamp: Long, version: Long)
  case class Proven(h: Header, sender: Recipient.Address, bodyBytes: ByteVector, senderPk: ByteVector, proofs: IndexedSeq[ByteVector])
  case class TransferItem(recipient: Recipient, amount: Long)
  case class Pmt(asset: Option[ByteVector], amount: Long)

  case class Genesis(header: Header, amount: Long, recipient: Recipient) extends Tx
  case class Payment(p: Proven, amount: Long, recipient: Recipient)      extends Tx
  case class Transfer(p: Proven,
                      feeAssetId: Option[ByteVector],
                      assetId: Option[ByteVector],
                      amount: Long,
                      recipient: Recipient,
                      attachment: ByteVector)
      extends Tx
  case class Issue(p: Proven,
                   quantity: Long,
                   name: ByteVector,
                   description: ByteVector,
                   reissuable: Boolean,
                   decimals: Long,
                   script: Option[ByteVector])
      extends Tx
  case class ReIssue(p: Proven, quantity: Long, assetId: ByteVector, reissuable: Boolean) extends Tx
  case class Burn(p: Proven, quantity: Long, assetId: ByteVector)                         extends Tx
  case class CI(p: Proven, contractAddress: Recipient, maybePayment: Option[Pmt])         extends Tx
  case class Lease(p: Proven, amount: Long, recipient: Recipient)                         extends Tx
  case class LeaseCancel(p: Proven, leaseId: ByteVector)                                  extends Tx
  case class CreateAlias(p: Proven, alias: String)                                        extends Tx
  case class SetScript(p: Proven, script: Option[ByteVector])                             extends Tx
  case class SetAssetScript(p: Proven, assetId: ByteVector, script: Option[ByteVector])   extends Tx
  case class MassTransfer(p: Proven,
                          assetId: Option[ByteVector],
                          transferCount: Long,
                          totalAmount: Long,
                          transfers: IndexedSeq[TransferItem],
                          attachment: ByteVector)
      extends Tx
  case class Sponsorship(p: Proven, assetId: ByteVector, minSponsoredAssetFee: Option[Long])                                          extends Tx
  case class Exchange(p: Proven, amount: Long, price: Long, buyMatcherFee: Long, sellMatcherFee: Long, buyOrder: Ord, sellOrder: Ord) extends Tx
  case class Data(p: Proven, data: IndexedSeq[DataItem[_]])                                                                           extends Tx
}
