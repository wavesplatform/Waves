package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty

object SetAssetScriptTransaction {
  val SET_ASSET_SCRIPT = 15
  private val MAX_TX_SIZE = KBYTE
}

class SetAssetScriptTransaction extends Nothing {
  final private var senderPublicKey = null
  final private var chainId = 0
  final private var script = null
  final private var assetId = null
  final private var fee = 0L
  final private var timestamp = 0L

  def this(senderPublicKey: Nothing, chainId: Byte, assetId: String, script: String, fee: Long, timestamp: Long) {
    this()
    this.senderPublicKey = senderPublicKey
    this.chainId = chainId
    this.assetId = assetId
    this.script = script
    this.fee = fee
    this.timestamp = timestamp
    this.proofs = Collections.unmodifiableList(Collections.singletonList(new ByteString(senderPublicKey.sign(getBodyBytes))))
  }

  def this(
            @JsonProperty("senderPublicKey") senderPublicKey: Nothing, @JsonProperty("chainId") chainId: Byte,
            @JsonProperty("assetId") assetId                : String, @JsonProperty("script") script: String,
            @JsonProperty("fee") fee                        : Long, @JsonProperty("timestamp") timestamp: Long,
            @JsonProperty("proofs") proofs                  : util.List[ByteString]
          ) {
    this()
    setProofs(proofs)
    this.senderPublicKey = senderPublicKey
    this.chainId = chainId
    this.assetId = assetId
    this.script = script
    this.fee = fee
    this.timestamp = timestamp
  }

  def getSenderPublicKey: Nothing = senderPublicKey

  def getChainId: Byte = chainId

  def getAssetId: String = Asset.toJsonObject(assetId)

  def getScript: String = script

  def getFee: Long = fee

  def getTimestamp: Long = timestamp

  def getTransactionMaxSize: Int = {
    val rawScript = if (script == null) new Array[Byte](0)
    else Base64.decode(script)
    SetAssetScriptTransaction.MAX_TX_SIZE + rawScript.length
  }

  def getBodyBytes: Array[Byte] = {
    val buf = ByteBuffer.allocate(getTransactionMaxSize)
    buf.put(SetAssetScriptTransaction.SET_ASSET_SCRIPT)
    buf.put(Transaction.V1)
    buf.put(chainId)
    buf.put(senderPublicKey.getPublicKey)
    buf.put(Base58.decode(assetId))
    buf.putLong(fee).putLong(timestamp)
    putScript(buf, script)
    ByteArraysUtils.getOnlyUsed(buf)
  }

  def getType: Byte = SetAssetScriptTransaction.SET_ASSET_SCRIPT

  def getVersion: Byte = Transaction.V1

  def withProof(index: Int, proof: ByteString): SetAssetScriptTransaction = {
    val newProofs = updateProofs(index, proof)
    new SetAssetScriptTransaction(senderPublicKey, chainId, assetId, script, fee, timestamp, newProofs)
  }

  def equals(o: Any): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[SetAssetScriptTransaction]
    if (getChainId != that.getChainId) return false
    if (getFee != that.getFee) return false
    if (getTimestamp != that.getTimestamp) return false
    if (if (getAssetId != null) !(getAssetId == that.getAssetId)
    else that.getAssetId != null) return false
    if (if (getSenderPublicKey != null) !getSenderPublicKey.equals(that.getSenderPublicKey)
    else that.getSenderPublicKey != null) return false
    if (getScript != null) getScript == that.getScript
    else that.getScript == null
  }

  def hashCode: Int = {
    var result = if (getSenderPublicKey != null) getSenderPublicKey.hashCode
    else 0
    result = 31 * result + getChainId.toInt
    result = 31 * result + (if (getAssetId != null) getAssetId.hashCode
    else 0)
    result = 31 * result + (if (getScript != null) getScript.hashCode
    else 0)
    result = 31 * result + (getFee ^ (getFee >>> 32)).toInt
    result = 31 * result + (getTimestamp ^ (getTimestamp >>> 32)).toInt
    result
  }
}