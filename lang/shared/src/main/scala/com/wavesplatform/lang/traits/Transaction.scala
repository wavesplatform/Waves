package com.wavesplatform.lang.traits

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
  def assetId: Either[String, Option[ByteVector]]
  def proofs: Either[String, IndexedSeq[ByteVector]]
  def recipient: Either[String, ByteVector]
  def alias_text: Either[String, String]
  def reissuable: Either[String, Boolean]
  def decimals: Either[String, Byte]
  def assetDescription: Either[String, ByteVector]
  def assetName: Either[String, ByteVector]
  def attachment: Either[String, ByteVector]
  def chainId: Either[String, Byte]
  def version: Either[String, Byte]
}
