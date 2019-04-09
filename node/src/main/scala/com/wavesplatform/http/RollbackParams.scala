package com.wavesplatform.http

import play.api.libs.json.{Format, Json}

object RollbackParams {
  implicit val rollbackParamsReads: Format[RollbackParams] = Json.format
}

case class RollbackParams(rollbackTo: Int, returnTransactionsToUtx: Boolean)
