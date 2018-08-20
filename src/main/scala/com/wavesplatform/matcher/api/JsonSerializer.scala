package com.wavesplatform.matcher.api
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

object JsonSerializer {

  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper.registerModule(DefaultScalaModule)

  def serialize(value: Any): String                             = mapper.writeValueAsString(value)
  def deserialize[T](value: String)(implicit m: Manifest[T]): T = mapper.readValue(value)

}
