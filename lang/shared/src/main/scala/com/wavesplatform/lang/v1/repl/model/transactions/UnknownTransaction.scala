package com.wavesplatform.lang.v1.repl.model.transactions

class UnknownTransaction @JsonCreator(

var senderPublicKey: Nothing, var timestamp: Long, var fee: Long, @JsonProperty ("signature") val signature: ByteString, var id: ByteString, var version: Byte, var `type`: Byte) extends Nothing {
  this.signature = signature
  def getId: ByteString = {
  return id
}
  def getBodyBytes: Array[Byte] = {
  throw new IllegalStateException ("Can't build the bytes for unknown transaction")
}
  def getSenderPublicKey: Nothing = {
  return senderPublicKey
}
  def getTimestamp: Long = {
  return timestamp
}
  def getFee: Long = {
  return fee
}
  def getVersion: Byte = {
  return version
}
  def getType: Byte = {
  return `type`
}
}