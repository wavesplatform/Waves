package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty

object ReissueTransactionV2 {
  val REISSUE = 5
  private val MAX_TX_SIZE = KBYTE
}

class ReissueTransactionV2 extends Nothing with ReissueTransaction {
  private var senderPublicKey = null
  private var chainId = 0
  private var assetId = null
  private var quantity = 0L
  private var reissuable = false
  private var fee = 0L
  private var timestamp = 0L

  def this(
            @JsonProperty("senderPublicKey") senderPublicKey: Nothing, @JsonProperty("chainId") chainId: Byte,
            @JsonProperty("address") assetId                : String, @JsonProperty("quantity") quantity: Long,
            @JsonProperty("reissuable") reissuable          : Boolean, @JsonProperty("fee") fee: Long,
            @JsonProperty("timestamp") timestamp            : Long,
            @JsonProperty("proofs") proofs                  : util.List[ByteString]
          ) {
    this()
    setProofs(proofs)
    this.senderPublicKey = senderPublicKey
    this.chainId = chainId
    this.assetId = assetId
    this.quantity = quantity
    this.reissuable = reissuable
    this.fee = fee
    this.timestamp = timestamp
  }

  def this(
            senderPublicKey: Nothing, chainId: Byte, assetId: String, quantity: Long, reissuable: Boolean, fee: Long,
            timestamp      : Long
          ) {
    this()
    this.senderPublicKey = senderPublicKey
    this.chainId = chainId
    this.assetId = assetId
    this.quantity = quantity
    this.reissuable = reissuable
    this.fee = fee
    this.timestamp = timestamp
    this.proofs = Collections.unmodifiableList(Collections.singletonList(new ByteString(senderPublicKey.sign(getBodyBytes))))
  }

  override def getSenderPublicKey: Nothing = senderPublicKey

  def getChainId: Byte = chainId

  override def getAssetId: String = assetId

  override def getQuantity: Long = quantity

  override def isReissuable: Boolean = reissuable

  override def getFee: Long = fee

  override def getTimestamp: Long = timestamp

  def getTransactionMaxSize: Int = ReissueTransactionV2.MAX_TX_SIZE

  override def getBodyBytes: Array[Byte] = {
    val buf = ByteBuffer.allocate(ReissueTransactionV2.MAX_TX_SIZE)
    buf.put(ReissueTransaction.REISSUE).put(Transaction.V2).put(chainId).put(senderPublicKey.getPublicKey).put(Base58.decode(assetId)).putLong(quantity).put((if (reissuable) 1
    else 0).toByte).putLong(fee).putLong(timestamp)
    ByteArraysUtils.getOnlyUsed(buf)
  }

  override def getType: Byte = ReissueTransactionV2.REISSUE

  override def getVersion: Byte = Transaction.V2

  override def equals(o: Any): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[ReissueTransactionV2]
    if (getChainId != that.getChainId) return false
    if (getQuantity != that.getQuantity) return false
    if (isReissuable != that.isReissuable) return false
    if (getFee != that.getFee) return false
    if (getTimestamp != that.getTimestamp) return false
    if (if (getSenderPublicKey != null) !getSenderPublicKey.equals(that.getSenderPublicKey)
    else that.getSenderPublicKey != null) return false
    if (getAssetId != null) getAssetId == that.getAssetId
    else that.getAssetId == null
  }

  override def hashCode: Int = {
    var result = if (getSenderPublicKey != null) getSenderPublicKey.hashCode
    else 0
    result = 31 * result + getChainId.toInt
    result = 31 * result + (if (getAssetId != null) getAssetId.hashCode
    else 0)
    result = 31 * result + (getQuantity ^ (getQuantity >>> 32)).toInt
    result = 31 * result + (if (isReissuable) 1
    else 0)
    result = 31 * result + (getFee ^ (getFee >>> 32)).toInt
    result = 31 * result + (getTimestamp ^ (getTimestamp >>> 32)).toInt
    result
  }

  def withProof(index: Int, proof: ByteString): ReissueTransactionV2 = {
    val newProofs = updateProofs(index, proof)
    new ReissueTransactionV2(senderPublicKey, chainId, assetId, quantity, reissuable, fee, timestamp, newProofs)
  }
}