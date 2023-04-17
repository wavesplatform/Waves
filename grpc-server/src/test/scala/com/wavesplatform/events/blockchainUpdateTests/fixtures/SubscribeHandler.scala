package com.wavesplatform.events.blockchainUpdateTests.fixtures

import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append
import io.grpc.StatusRuntimeException

class SubscribeHandler {
  private var txIndex = 0
  private var append: Append = new Append()

  def subscribeResponseHandler(txId: Array[Byte], subscribeEvent: Iterator[SubscribeEvent]): Unit = {
    try {
      while (subscribeEvent.hasNext) {
        append = subscribeEvent.next().getUpdate.getAppend
        subscribeEventHandler(txId)
      }
    } catch {
      case _: StatusRuntimeException => ()
    }
  }

  private def subscribeEventHandler(txId: Array[Byte]): Unit = {
    val transactionIdsCount = append.transactionIds.size

    for (i <- 0 until transactionIdsCount) {
      val transactionId = append.transactionIds.apply(i).toByteArray
      if (transactionId == txId) {
        txIndex = i
      }
    }
  }

  def getTxIndex: Int = txIndex

  def getAppend: Append = append
}
