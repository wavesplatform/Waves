package com.wavesplatform.lang.v1.repl.model.order

import com.wavesplatform.lang.v1.repl.model.transaction.ByteString
import com.wavesplatform.lang.v1.repl.model.{Account, AssetPair, OrderType, WithProofs, WithSignature}

case class OrderV2(
  matcherPublicKey: Account,
  assetPair: AssetPair,
  orderType: OrderType,
  amount: Long,
  price: Long,
  timestamp: Long,
  expiration: Long,
  matcherFee: Long,
  proofs: List[ByteString],
  version: Byte,
  senderPublicKey: Account
) extends Order with WithProofs