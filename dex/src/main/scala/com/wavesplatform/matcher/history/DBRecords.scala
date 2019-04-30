package com.wavesplatform.matcher.history

object DBRecords {

  sealed trait Record

  case class OrderRecord(id: String,
                         sender: String,
                         senderPublicKey: String,
                         amountAssetId: String,
                         priceAssetId: String,
                         side: Byte,
                         price: Double,
                         amount: Double,
                         timestamp: Long,
                         expiration: Long,
                         fee: Double,
                         created: Long)
      extends Record

  case class EventRecord(orderId: String, eventType: Byte, timestamp: Long, price: Double, filled: Double, totalFilled: Double, status: Byte)
      extends Record

}
