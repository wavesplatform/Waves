package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import play.api.libs.json.{Format, Writes}
import supertagged._

object PrivateKey extends TaggedType[ByteStr] {
  def apply(privateKey: ByteStr): PrivateKey = {
    privateKey @@ PrivateKey
  }

  def apply(privateKey: Array[Byte]): PrivateKey =
    apply(ByteStr(privateKey))

  def unapply(arg: Array[Byte]): Option[PrivateKey] =
    Some(apply(arg))

  implicit lazy val jsonFormat: Format[PrivateKey] = Format[PrivateKey](
    com.wavesplatform.utils.byteStrWrites.map(this.apply),
    Writes(pk => com.wavesplatform.utils.byteStrWrites.writes(pk))
  )
}
