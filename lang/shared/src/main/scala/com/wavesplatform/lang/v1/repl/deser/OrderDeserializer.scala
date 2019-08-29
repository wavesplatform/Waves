package com.wavesplatform.lang.v1.repl.deser

import com.fasterxml.jackson.core.{JsonParser, TreeNode}
import com.fasterxml.jackson.databind.deser.std.StdDeserializer
import com.fasterxml.jackson.databind.{DeserializationContext, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.wavesplatform.lang.v1.repl.model.order.{Order, OrderV1, OrderV2}

case class OrderDeserializer(objectMapper: ObjectMapper with ScalaObjectMapper) extends StdDeserializer(classOf[Order]) {
  override def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): Order = {
    val treeNode: TreeNode = jsonParser.getCodec.readTree(jsonParser)
    val version  = objectMapper.treeToValue[Int](treeNode.get("version"))
    val c = version match {
      case Order.V1 => classOf[OrderV1]
      case Order.V2 => classOf[OrderV2]
    }
    objectMapper.treeToValue(treeNode, c).asInstanceOf[Order]
  }
}
