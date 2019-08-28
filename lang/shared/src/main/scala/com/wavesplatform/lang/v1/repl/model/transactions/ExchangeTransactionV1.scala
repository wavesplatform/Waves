package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, Order, WithSignature}

case class ExchangeTransactionV1(
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
    version: Byte
) extends ExchangeTransaction with WithSignature
