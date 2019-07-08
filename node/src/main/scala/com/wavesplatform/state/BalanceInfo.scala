package com.wavesplatform.state

import play.api.libs.json.Json

case class BalanceInfo(regularBalance: Long, miningBalance: Long)

object BalanceInfo {
  implicit val writes = Json.writes[BalanceInfo]
}
