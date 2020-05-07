package com.wavesplatform.account

import com.wavesplatform.common.state.ByteStr
import play.api.libs.json.{Format, Writes}
import supertagged._
import com.wavesplatform.crypto.KeyLength

object PrivateKey extends TaggedType[ByteStr] {
  def apply(privateKey: ByteStr): PrivateKey = {
    require(privateKey.arr.length == KeyLength, s"invalid public key length: ${privateKey.arr.length}")
    privateKey @@ PrivateKey
  }

  def apply(privateKey: Array[Byte]): PrivateKey =
    apply(ByteStr(privateKey))

  def unapply(arg: Array[Byte]): Option[PrivateKey] =
    Some(apply(arg))

  implicit lazy val jsonFormat: Format[PrivateKey] = Format[PrivateKey](
    com.wavesplatform.utils.byteStrFormat.map(this.apply),
    Writes(pk => com.wavesplatform.utils.byteStrFormat.writes(pk))
  )
}
