package com.wavesplatform.lang.v1.repl.model.order
import com.wavesplatform.lang.v1.repl.model.{Account, AssetPair, OrderType, WithSignature}
import com.wavesplatform.lang.v1.repl.model.transaction.ByteString

case class OrderV1(
  matcherPublicKey: Account,
  assetPair: AssetPair,
  orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long,
  version: Byte,
  senderPublicKey: Account,
  signature: ByteString,
) extends Order with WithSignature {
  override val proofs: List[ByteString] = List(signature)
}