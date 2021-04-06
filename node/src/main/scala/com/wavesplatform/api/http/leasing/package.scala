package com.wavesplatform.api.http

import com.wavesplatform.api.common.LeaseInfo
import play.api.libs.json.{Json, OWrites}

package object leasing {
  implicit lazy val leaseInfoWrites: OWrites[LeaseInfo] = {
    import com.wavesplatform.utils.byteStrFormat
    Json.writes[LeaseInfo]
  }
}
