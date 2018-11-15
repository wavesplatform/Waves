package com.wavesplatform.matcher.model

import java.util.UUID

import com.wavesplatform.account.Address
import com.wavesplatform.matcher.model.MatcherModel.OrderId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}

sealed trait Command {
  def id: Command.Id
}

object Command {
  type Id = UUID
  def newId: UUID = UUID.randomUUID()

  case class Place(id: Id, order: Order)                   extends Command
  case class Cancel(id: Id, orderId: OrderId)              extends Command
  case class CancelAllInPair(id: Id, assetPair: AssetPair) extends Command
  case class CancelAll(id: Id, address: Address)           extends Command
  case class DeleteOrderBook(id: Id, assetPair: AssetPair) extends Command
}
