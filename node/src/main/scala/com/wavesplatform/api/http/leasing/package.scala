package com.wavesplatform.api.http

import com.wavesplatform.api.common.LeaseInfo
import play.api.libs.json.{Json, JsString, OWrites, Writes}

package object leasing {
  implicit lazy val leaseInfoWrites: OWrites[LeaseInfo] = {
    import com.wavesplatform.api.common.LeaseInfo.Status
    import com.wavesplatform.utils.byteStrFormat

    implicit val statusWrites: Writes[Status] = Writes(st => JsString(st.toString.toLowerCase))

    Json.writes[LeaseInfo]
  }
}
