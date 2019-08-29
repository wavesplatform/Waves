package com.wavesplatform.lang.v1.repl

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.repl.deser.{ByteStringDeserializer, TransactionDeserializer}
import com.wavesplatform.lang.v1.repl.model.Transaction
import com.wavesplatform.lang.v1.repl.model.transactions.ByteString
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}

case class EnvironmentHttpClient(settings: NodeConnectionSettings) extends Environment {
  private val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper
    .registerModule(DefaultScalaModule)
    .registerModule(new SimpleModule()
      .addDeserializer(classOf[Transaction], TransactionDeserializer(mapper, settings.networkByte))
      .addDeserializer(classOf[ByteString], ByteStringDeserializer)
    )

  val url = settings.url

  override def height: Long = ???

  override def chainId: Byte = settings.networkByte

  override def inputEntity: InputEntity = ???

  override def tthis: Recipient.Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def transactionById(id: Array[Byte]): Option[Tx] = ???

  override def transferTransactionById(id: Array[Byte]): Option[Tx] = ???

  override def transactionHeightById(id: Array[Byte]): Option[Long] = ???

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = ???

  override def lastBlockOpt(): Option[BlockInfo] = ???

  override def blockInfoByHeight(height: Int): Option[BlockInfo] = ???

  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] = ???

  override def resolveAlias(name: String): Either[String, Recipient.Address] = ???

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
}
