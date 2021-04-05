package com.wavesplatform.api.http

import com.wavesplatform.api.common.LeaseInfo
import play.api.libs.json.{Json, JsString, OWrites, Writes}

package object leasing {
  implicit lazy val leaseInfoWrites: OWrites[LeaseInfo] = {
    import com.wavesplatform.api.common.LeaseInfo.{Status, TransactionRef}
    import com.wavesplatform.utils.byteStrFormat

    implicit val statusWrites: Writes[Status]                 = Writes(st => JsString(st.toString.toLowerCase))
    implicit val transactionRefWrites: OWrites[TransactionRef] = Json.writes[TransactionRef]

    Json.writes[LeaseInfo]
  }
}
