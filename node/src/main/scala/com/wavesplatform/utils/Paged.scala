package com.wavesplatform.utils

import play.api.libs.json._

final case class Paged[C, R](hasNext: Boolean, lastItem: Option[C], items: R)

object Paged {
  implicit def pagedWrites[C: Writes, R: Writes]: Writes[Paged[C, R]] = Json.writes[Paged[C, R]]
}
