package com.wavesplatform.ride

import com.softwaremill.diffx.Diff
import com.softwaremill.diffx.generic.auto.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.script.Script
import com.wavesplatform.ride.runner.input.StringOrBytesAsByteArray

trait DiffXInstances {
  implicit val diffForByteStr: Diff[ByteStr]                                   = Diff.summon[String].contramap(_.toString)
  implicit val diffForStringOrBytesAsByteArray: Diff[StringOrBytesAsByteArray] = diffForByteStr.contramap(ByteStr(_))
  implicit val diffForScript: Diff[Script]                                     = diffForByteStr.contramap(_.bytes())
  implicit val diffForPublicKey: Diff[PublicKey]                               = diffForByteStr.contramap(x => x)
}
