package com.wavesplatform.it.api

import com.wavesplatform.account.KeyPair
import com.wavesplatform.matcher.queue.QueueEventWithMeta
import com.wavesplatform.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, OrderBookResponse],
                        orderStatuses: Map[String, MatcherStatusResponse],
                        reservedBalances: Map[KeyPair, Map[String, Long]],
                        orderHistory: Map[KeyPair, Map[AssetPair, Seq[OrderbookHistory]]])
