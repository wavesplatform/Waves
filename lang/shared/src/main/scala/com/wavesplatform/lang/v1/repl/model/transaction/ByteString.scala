package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.common.utils.{Base58, Base64}

case class ByteString(bytes: Array[Byte] = Array(0))
object ByteString {
  implicit val r =
    upickle.default.reader[String]
      .map[ByteString](str =>
        if (str.startsWith("base58:")) ByteString(Base58.decode(str.substring(7)))
        else if (str.startsWith("base64:")) ByteString(Base64.decode(str.substring(7)))
        else ByteString(Base58.decode(str))
      )
}