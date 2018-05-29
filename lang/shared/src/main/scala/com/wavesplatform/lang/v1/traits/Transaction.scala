package com.wavesplatform.lang.v1.traits

import scodec.bits.ByteVector

case class Header(id: ByteVector, fee: Long, timestamp: Long, version: Long)

case class Proven(h: Header, bodyBytes: ByteVector, senderPk: ByteVector, proofs: IndexedSeq[ByteVector])

trait Recipient
object Recipient {
  case class Address(bytes: ByteVector) extends Recipient
  case class Alias(name: String)        extends Recipient
}
case class TransferItem(recipient: Recipient, amount: Long)

trait Tx

object Tx {

  case class Genesis(header: Header, amount: Long, recipient: Recipient) extends Tx
  case class Transfer(p: Proven,
                      feeAssetId: Option[ByteVector],
                      transferAssetId: Option[ByteVector],
                      amount: Long,
                      recipient: Recipient,
                      attachment: ByteVector)
      extends Tx
  case class Issue(p: Proven, amount: Long, assetName: ByteVector, assetDescription: ByteVector, reissuable: Boolean)                  extends Tx
  case class ReIssue(p: Proven, amount: Long, reissuable: Boolean)                                                                     extends Tx
  case class Burn(p: Proven, amount: Long)                                                                                             extends Tx
  case class Lease(p: Proven, amount: Long, recipient: Recipient)                                                                      extends Tx
  case class LeaseCancel(p: Proven, leaseId: ByteVector)                                                                               extends Tx
  case class CreateAlias(p: Proven, alias: String)                                                                                     extends Tx
  case class MassTransfer(p: Proven, transferAssetId: Option[ByteVector], transfers: IndexedSeq[TransferItem], attachment: ByteVector) extends Tx
}
