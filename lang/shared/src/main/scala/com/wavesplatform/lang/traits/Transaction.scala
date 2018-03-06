package com.wavesplatform.lang.traits

import scodec.bits.ByteVector

trait Transaction {
  def transactionType: Int
  def id: ByteVector
  def bodyBytes: Either[String, ByteVector]
  def senderPk: Either[String, ByteVector]
  def assetId: Either[String, Option[ByteVector]]
  def proofs: Either[String,IndexedSeq[ByteVector]]
}
