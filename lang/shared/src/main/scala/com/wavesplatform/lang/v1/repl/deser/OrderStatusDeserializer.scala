package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.JsonParser
import com.fasterxml.jackson.databind.{DeserializationContext, JsonDeserializer}
import com.wavesplatform.lang.v1.repl.model.OrderStatus

object OrderStatusDeserializer extends JsonDeserializer[OrderStatus] {
  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): OrderStatus =
    OrderStatus.fromString(jsonParser.getValueAsString)
}
