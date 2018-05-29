package com.wavesplatform.lang.v1.traits

import scodec.bits.ByteVector

trait Transaction {
  def transactionType: Int
  def id: ByteVector
  def fee: Long
  def amount: Either[String, Long]
  def feeAssetId: Option[ByteVector]
  def timestamp: Long
  def bodyBytes: Either[String, ByteVector]
  def senderPk: Either[String, ByteVector]
  def transferAssetId: Either[String, Option[ByteVector]]
  def assetId: Either[String, ByteVector]
  def proofs: Either[String, IndexedSeq[ByteVector]]
  def recipient: Either[String, Recipient]
  def alias: Either[String, String]
  def reissuable: Either[String, Boolean]
  def leaseId: Either[String, ByteVector]
  def decimals: Either[String, Byte]
  def assetDescription: Either[String, ByteVector]
  def assetName: Either[String, ByteVector]
  def attachment: Either[String, ByteVector]
  def chainId: Either[String, Byte]
  def version: Either[String, Byte]
  def minSponsoredAssetFee: Either[String, Option[Long]]
  def transfers: Either[String, IndexedSeq[Transfer]]
}

trait Recipient
object Recipient {
  case class Address(bytes: ByteVector) extends Recipient
  case class Alias(name: String)        extends Recipient
}
case class Transfer(recipient: Recipient, amount: Long)
