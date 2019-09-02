package com.wavesplatform.lang.v1.repl.model.transaction

import java.nio.ByteBuffer
import java.util
import java.util.Collections

import com.fasterxml.jackson.annotation.JsonProperty
import com.wavesplatform.lang.v1.repl.model.order.Order
import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithProofs, WithSignature}

object ExchangeTransactionV2 {
  val EXCHANGE = 7
  val MAX_TX_SIZE = 1024
}

case class ExchangeTransactionV2(
   id: ByteString,
   amount: Long,
   price: Long,
   buyMatcherFee: Long,
   sellMatcherFee: Long,
   order1: Order,
   order2: Order,
   signature: ByteString,
   senderPublicKey: Account,
   fee: Long,
   timestamp: Long,
   height: Int,
   `type`: Byte,
   version: Byte,
   proofs: List[ByteString]
) extends ExchangeTransaction with WithProofs
