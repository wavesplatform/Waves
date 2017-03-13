package com.wavesplatform.state2

import cats._
import cats.implicits._
import scorex.account.Account
import scorex.transaction.Transaction

case class Diff(transactions: Map[ByteArray, (Int, Transaction)],
                portfolios: Map[Account, Portfolio],
                issuedAssets: Map[ByteArray, AssetInfo],
                height : Int
               )

case class Portfolio(balance: Long, effectiveBalance: Long, assets: Map[ByteArray, Long])

case class AssetInfo(isReissuableOverride: Boolean, totalVolumeOverride: Long)
