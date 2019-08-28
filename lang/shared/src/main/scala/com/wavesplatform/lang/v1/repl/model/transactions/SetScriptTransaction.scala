package com.wavesplatform.lang.v1.repl.model.transactions

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty

object SetScriptTransaction {
  val SET_SCRIPT = 13
  private val MAX_TX_SIZE = 10 * KBYTE
}

class SetScriptTransaction extends Nothing {
  private var senderPublicKey = null
  private var script = null
  private var chainId = 0
  private var fee = 0L
  private var timestamp = 0L

  def this(
            @JsonProperty("senderPublicKey") senderPublicKey: Nothing, @JsonProperty("script") script: String,
            @JsonProperty("chainId") chainId                : Byte, @JsonProperty("fee") fee: Long,
            @JsonProperty("timestamp") timestamp            : Long,
            @JsonProperty("proofs") proofs                  : util.List[ByteString]
          ) {
    this()
    setProofs(proofs)
    this.senderPublicKey = senderPublicKey
    this.script = script
    this.chainId = chainId
    this.fee = fee
    this.timestamp = timestamp
  }

  def this(senderPublicKey: Nothing, script: String, chainId: Byte, fee: Long, timestamp: Long) {
    this()
    this.senderPublicKey = senderPublicKey
    this.script = script
    this.chainId = chainId
    this.fee = fee
    this.timestamp = timestamp
    this.proofs = Collections.unmodifiableList(Collections.singletonList(new ByteString(senderPublicKey.sign(getBodyBytes))))
  }

  def getSenderPublicKey: Nothing = senderPublicKey

  def getScript: String = script

  def getChainId: Byte = chainId

  def getFee: Long = fee

  def getTimestamp: Long = timestamp

  def getBodyBytes: Array[Byte] = {
    val rawScript = if (script == null) new Array[Byte](0)
    else Base64.decode(script)
    val buf = ByteBuffer.allocate(KBYTE + rawScript.length)
    buf.put(SetScriptTransaction.SET_SCRIPT).put(Transaction.V1).put(chainId)
    buf.put(senderPublicKey.getPublicKey)
    if (rawScript.length > 0) buf.put(1.toByte).putShort(rawScript.length.toShort).put(rawScript)
    else buf.put(0.toByte)
    buf.putLong(fee).putLong(timestamp)
    ByteArraysUtils.getOnlyUsed(buf)
  }

  def getType: Byte = SetScriptTransaction.SET_SCRIPT

  def getVersion: Byte = Transaction.V1

  def equals(o: Any): Boolean = {
    if (this eq o) return true
    if (o == null || (getClass ne o.getClass)) return false
    val that = o.asInstanceOf[SetScriptTransaction]
    if (getChainId != that.getChainId) return false
    if (getFee != that.getFee) return false
    if (getTimestamp != that.getTimestamp) return false
    if (if (getSenderPublicKey != null) !getSenderPublicKey.equals(that.getSenderPublicKey)
    else that.getSenderPublicKey != null) return false
    if (getScript != null) getScript == that.getScript
    else that.getScript == null
  }

  def hashCode: Int = {
    var result = if (getSenderPublicKey != null) getSenderPublicKey.hashCode
    else 0
    result = 31 * result + (if (getScript != null) getScript.hashCode
    else 0)
    result = 31 * result + getChainId.toInt
    result = 31 * result + (getFee ^ (getFee >>> 32)).toInt
    result = 31 * result + (getTimestamp ^ (getTimestamp >>> 32)).toInt
    result
  }

  def withProof(index: Int, proof: ByteString): SetScriptTransaction = {
    val newProofs = updateProofs(index, proof)
    new SetScriptTransaction(senderPublicKey, script, chainId, fee, timestamp, newProofs)
  }
}