package com.wavesplatform.matcher.market

import akka.actor.Actor
import com.wavesplatform.matcher.market.OrderRepoActor.{AddOrderAmountRequest, GetOpenOrderAmountResponse, GetOrdersAmountRequest}
import scorex.transaction.AssetAcc
import scorex.transaction.state.database.state.Address

object OrderRepoActor {
  case class GetOrdersAmountRequest(assetAcc: AssetAcc)
  case class AddOrderAmountRequest(assetAcc: AssetAcc, amount: Long)
  case class GetOpenOrderAmountResponse(amount: Long)
}

class OrderRepoActor extends Actor {
  var spendAssetToOrders = Map.empty[Address, Long]

  override def receive: Receive = {
    case GetOrdersAmountRequest(assetAcc) =>
      sender() ! GetOpenOrderAmountResponse(spendAssetToOrders.getOrElse(assetAcc.key, 0L))
    case AddOrderAmountRequest(assetAcc: AssetAcc, amount: Long) =>
      spendAssetToOrders += assetAcc.key -> (spendAssetToOrders.getOrElse(assetAcc.key, 0L) + amount)
  }
}