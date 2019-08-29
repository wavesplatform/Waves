package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer}
import com.wavesplatform.lang.v1.repl.model.Alias

case class AliasDeserializer(chainId: Byte) extends JsonDeserializer[Alias] {
  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): Alias =
    Alias(jsonParser.getValueAsString, chainId)
}

