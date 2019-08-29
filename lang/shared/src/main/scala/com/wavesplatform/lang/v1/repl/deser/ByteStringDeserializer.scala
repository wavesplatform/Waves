package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.DeserializationContext
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.wavesplatform.common.utils.{Base58, Base64}
import com.wavesplatform.lang.v1.repl.model.transactions.ByteString

object ByteStringDeserializer extends StdDeserializer[ByteString] {
  override def deserialize(p: JsonParser, ctxt: DeserializationContext): ByteString = {
    val str = p.getValueAsString
    if (str.startsWith("base58:")) ByteString(Base58.decode(str.substring(7)))
    else if (str.startsWith("base64:")) ByteString(Base64.decode(str.substring(7)))
    else ByteString(Base58.decode(str))
  }
}
