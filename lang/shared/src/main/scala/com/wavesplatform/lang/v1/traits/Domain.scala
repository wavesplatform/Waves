package com.wavesplatform.lang.v1.traits

import scodec.bits.ByteVector

case class Header(id: ByteVector, fee: Long, timestamp: Long, version: Long)

case class Proven(h: Header, sender: Recipient.Address, bodyBytes: ByteVector, senderPk: ByteVector, proofs: IndexedSeq[ByteVector])

trait Recipient
object Recipient {
  case class Address(bytes: ByteVector) extends Recipient
  case class Alias(name: String)        extends Recipient
}
case class TransferItem(recipient: Recipient, amount: Long)

sealed trait OrdType
object OrdType {
  case object Buy  extends OrdType
  case object Sell extends OrdType
}

case class APair(amountAsset: Option[ByteVector], priceAsset: Option[ByteVector])

trait DataItem[T] {
  val key: String
  val value: T
}

object DataItem {
  case class Lng(k: String, v: Long)       extends DataItem[Long]       { val key = k; val value = v }
  case class Bool(k: String, v: Boolean)   extends DataItem[Boolean]    { val key = k; val value = v }
  case class Bin(k: String, v: ByteVector) extends DataItem[ByteVector] { val key = k; val value = v }
  case class Str(k: String, v: String)     extends DataItem[String]     { val key = k; val value = v }
}

case class Blk(timestamp: Long, height: Long, generationSignature: ByteVector)

case class Ord(id: ByteVector,
               sender: Recipient.Address,
               senderPublicKey: ByteVector,
               matcherPublicKey: ByteVector,
               assetPair: APair,
               orderType: OrdType,
               price: Long,
               amount: Long,
               timestamp: Long,
               expiration: Long,
               matcherFee: Long,
               bodyBytes: ByteVector,
               proofs: IndexedSeq[ByteVector])
trait Tx
object Tx {

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
  case class Lease(p: Proven, amount: Long, recipient: Recipient)                         extends Tx
  case class LeaseCancel(p: Proven, leaseId: ByteVector)                                  extends Tx
  case class CreateAlias(p: Proven, alias: String)                                        extends Tx
  case class SetScript(p: Proven, script: Option[ByteVector])                             extends Tx
  case class MassTransfer(p: Proven,
                          assetId: Option[ByteVector],
                          transferCount: Long,
                          totalAmount: Long,
                          transfers: IndexedSeq[TransferItem],
                          attachment: ByteVector)
      extends Tx
  case class Sponsorship(p: Proven, assetId: ByteVector, minSponsoredAssetFee: Option[Long])                                          extends Tx
  case class Exchange(p: Proven, price: Long, amount: Long, buyMatcherFee: Long, sellMatcherFee: Long, buyOrder: Ord, sellOrder: Ord) extends Tx
  case class Data(p: Proven, data: IndexedSeq[DataItem[_]])                                                                           extends Tx
}
