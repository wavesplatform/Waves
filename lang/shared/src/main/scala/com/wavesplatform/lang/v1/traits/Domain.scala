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

sealed trait OrdType
object OrdType {
  case object Buy  extends OrdType
  case object Sell extends OrdType
}

case class APair(amountAsset: Option[ByteVector], priceAsset: Option[ByteVector])

trait DataItem
object DataItem {
  case class Lng(k: String, v: Long)       extends DataItem
  case class Bool(k: String, v: Boolean)   extends DataItem
  case class Bin(k: String, v: ByteVector) extends DataItem
  case class Str(k: String, v: String)     extends DataItem
}

case class Ord(senderPublicKey: ByteVector,
               matcherPublicKey: ByteVector,
               assetPair: APair,
               orderType: OrdType,
               price: Long,
               amount: Long,
               timestamp: Long,
               expiration: Long,
               matcherFee: Long,
               signature: ByteVector)
trait Tx
object Tx {

  case class Genesis(header: Header, amount: Long, recipient: Recipient) extends Tx
  case class Payment(p: Proven, amount: Long, recipient: Recipient)      extends Tx
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
  case class SetScript(p: Proven, scipt: Option[ByteVector])                                                                           extends Tx
  case class MassTransfer(p: Proven, transferAssetId: Option[ByteVector], transfers: IndexedSeq[TransferItem], attachment: ByteVector) extends Tx
  case class Sponsorship(p: Proven, minFee: Option[Long])                                                                              extends Tx
  case class Exchange(p: Proven, price: Long, amount: Long, buyMatcherFee: Long, sellMatcherFee: Long, buyOrder: Ord, sellOrder: Ord)  extends Tx
  case class Data(p: Proven, items: IndexedSeq[DataItem])                                                                              extends Tx
}
