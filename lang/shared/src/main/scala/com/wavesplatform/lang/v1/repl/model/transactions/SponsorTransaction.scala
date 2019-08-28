package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty

object SponsorTransaction {
  val SPONSOR = 14
}

class SponsorTransaction extends Nothing {
  private var senderPublicKey = null
  private var assetId = null
  private var minSponsoredAssetFee = 0L
  private var fee = 0L
  private var timestamp = 0L

  def this(@JsonProperty("senderPublicKey") senderPublicKey          : Nothing,
           @JsonProperty("assetId") assetId                          : String,
           @JsonProperty("minSponsoredAssetFee") minSponsoredAssetFee: Long, @JsonProperty("fee") fee: Long,
           @JsonProperty("timestamp") timestamp                      : Long,
           @JsonProperty("proofs") proofs                            : util.List[ByteString]
          ) {
    this()
    setProofs(proofs)
    this.senderPublicKey = senderPublicKey
    this.assetId = assetId
    this.minSponsoredAssetFee = minSponsoredAssetFee
    this.fee = fee
    this.timestamp = timestamp
  }

  def this(senderPublicKey: Nothing, assetId: String, minSponsoredAssetFee: Long, fee: Long, timestamp: Long) {
    this()
    this.senderPublicKey = senderPublicKey
    this.assetId = assetId
    this.minSponsoredAssetFee = minSponsoredAssetFee
    this.fee = fee
    this.timestamp = timestamp
    this.proofs = Collections.unmodifiableList(Collections.singletonList(new ByteString(senderPublicKey.sign(getBodyBytes))))
  }

  def getSenderPublicKey: Nothing = senderPublicKey

  def getAssetId: String = Asset.toJsonObject(assetId)

  def getMinSponsoredAssetFee: Long = minSponsoredAssetFee

  def getFee: Long = fee

  def getTimestamp: Long = timestamp

  def getBodyBytes: Array[Byte] = {
    val buf = ByteBuffer.allocate(KBYTE)
    buf.put(SponsorTransaction.SPONSOR).put(Transaction.V1).put(senderPublicKey.getPublicKey).put(Base58.decode(assetId)).putLong(minSponsoredAssetFee).putLong(fee).putLong(timestamp)
    ByteArraysUtils.getOnlyUsed(buf)
  }

  def getType: Byte = SponsorTransaction.SPONSOR

  def getVersion: Byte = Transaction.V1

  def equals(o: Any): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[SponsorTransaction]
    if (getMinSponsoredAssetFee != that.getMinSponsoredAssetFee) return false
    if (getFee != that.getFee) return false
    if (getTimestamp != that.getTimestamp) return false
    if (if (getSenderPublicKey != null) !getSenderPublicKey.equals(that.getSenderPublicKey)
    else that.getSenderPublicKey != null) return false
    if (getAssetId != null) getAssetId == that.getAssetId
    else that.getAssetId == null
  }

  def hashCode: Int = {
    var result = if (getSenderPublicKey != null) getSenderPublicKey.hashCode
    else 0
    result = 31 * result + (if (getAssetId != null) getAssetId.hashCode
    else 0)
    result = 31 * result + (getMinSponsoredAssetFee ^ (getMinSponsoredAssetFee >>> 32)).toInt
    result = 31 * result + (getFee ^ (getFee >>> 32)).toInt
    result = 31 * result + (getTimestamp ^ (getTimestamp >>> 32)).toInt
    result
  }

  def withProof(index: Int, proof: ByteString): Transaction = {
    val newProofs = updateProofs(index, proof)
    new SponsorTransaction(senderPublicKey, assetId, minSponsoredAssetFee, fee, timestamp, newProofs)
  }
}