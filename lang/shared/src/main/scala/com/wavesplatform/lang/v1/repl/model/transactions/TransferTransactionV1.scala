package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer

import com.fasterxml.jackson.annotation.JsonProperty

class TransferTransactionV1 extends Nothing with TransferTransaction {
  final private var senderPublicKey = null
  final private var recipient = null
  final private var amount = 0L
  final private var assetId = null
  final private var fee = 0L
  final private var feeAssetId = null
  final private var attachment = null
  final private var timestamp = 0L

  def this(
            @JsonProperty("senderPublicKey") senderPublicKey: Nothing, @JsonProperty("recipient") recipient: String,
            @JsonProperty("amount") amount                  : Long, @JsonProperty("address") assetId: String,
            @JsonProperty("fee") fee                        : Long, @JsonProperty("feeAssetId") feeAssetId: String,
            @JsonProperty("attachment") attachment          : ByteString, @JsonProperty("timestamp") timestamp: Long,
            @JsonProperty("signature") signature            : ByteString
          ) {
    this()
    this.senderPublicKey = senderPublicKey
    this.recipient = recipient
    this.amount = amount
    this.assetId = assetId
    this.fee = fee
    this.feeAssetId = feeAssetId
    this.attachment = attachment
    this.timestamp = timestamp
    this.signature = signature
  }

  def this(
            senderPublicKey: Nothing, recipient: String, amount: Long, assetId: String, fee: Long, feeAssetId: String,
            attachment     : ByteString, timestamp: Long
          ) {
    this()
    this.senderPublicKey = senderPublicKey
    this.recipient = recipient
    this.amount = amount
    this.assetId = assetId
    this.fee = fee
    this.feeAssetId = feeAssetId
    this.attachment = attachment
    this.timestamp = timestamp
    this.signature = new ByteString(senderPublicKey.sign(getBodyBytes))
  }

  override def getSenderPublicKey: Nothing = senderPublicKey

  override def getRecipient: String = recipient

  override def getAmount: Long = amount

  override def getAssetId: String = Asset.toJsonObject(assetId)

  override def getFee: Long = fee

  override def getFeeAssetId: String = Asset.toJsonObject(feeAssetId)

  override def getAttachment: ByteString = attachment

  override def getTimestamp: Long = timestamp

  override def getBodyBytes: Array[Byte] = {
    val buf = ByteBuffer.allocate(KBYTE)
    buf.put(TransferTransaction.TRANSFER)
    buf.put(senderPublicKey.getPublicKey)
    putAsset(buf, assetId)
    putAsset(buf, feeAssetId)
    buf.putLong(timestamp).putLong(amount).putLong(fee)
    putRecipient(buf, senderPublicKey.getChainId, recipient)
    if (attachment != null) putBytes(buf, attachment.getBytes)
    else buf.put(0.toByte)
    ByteArraysUtils.getOnlyUsed(buf)
  }

  override def getType: Byte = TRANSFER

  override def getVersion: Byte = Transaction.V1

  override def equals(o: Any): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[TransferTransactionV1]
    if (getAmount != that.getAmount) return false
    if (getFee != that.getFee) return false
    if (getTimestamp != that.getTimestamp) return false
    if (if (getSenderPublicKey != null) !getSenderPublicKey.equals(that.getSenderPublicKey)
    else that.getSenderPublicKey != null) return false
    if (if (getRecipient != null) !(getRecipient == that.getRecipient)
    else that.getRecipient != null) return false
    if (if (getAssetId != null) !(getAssetId == that.getAssetId)
    else that.getAssetId != null) return false
    if (if (getFeeAssetId != null) !(getFeeAssetId == that.getFeeAssetId)
    else that.getFeeAssetId != null) return false
    if (getAttachment != null) getAttachment == that.getAttachment
    else that.getAttachment == null
  }

  override def hashCode: Int = {
    var result = if (getSenderPublicKey != null) getSenderPublicKey.hashCode
    else 0
    result = 31 * result + (if (getRecipient != null) getRecipient.hashCode
    else 0)
    result = 31 * result + (getAmount ^ (getAmount >>> 32)).toInt
    result = 31 * result + (if (getAssetId != null) getAssetId.hashCode
    else 0)
    result = 31 * result + (getFee ^ (getFee >>> 32)).toInt
    result = 31 * result + (if (getFeeAssetId != null) getFeeAssetId.hashCode
    else 0)
    result = 31 * result + (if (getAttachment != null) getAttachment.hashCode
    else 0)
    result = 31 * result + (getTimestamp ^ (getTimestamp >>> 32)).toInt
    result
  }
}