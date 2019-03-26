package com.wavesplatform.it.matcher

import com.wavesplatform.account.AccountKeyPair
import com.wavesplatform.it.Node
import com.wavesplatform.transaction.assets.exchange.Order

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(node: Node, order: Order)                         extends MatcherCommand
  case class Cancel(node: Node, owner: AccountKeyPair, order: Order) extends MatcherCommand
}
