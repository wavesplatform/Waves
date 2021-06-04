package com.wavesplatform.state

import play.api.libs.json.{Json, OFormat}

package object patch {
  implicit val leaseBalanceFormat: OFormat[LeaseBalance] = Json.format[LeaseBalance]
}
