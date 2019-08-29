package com.wavesplatform.lang.v1.repl

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.v1.repl.deser._
import com.wavesplatform.lang.v1.repl.model.order.Order
import com.wavesplatform.lang.v1.repl.model.{Account, Alias, OrderStatus, Request, Transaction}
import com.wavesplatform.lang.v1.repl.model.transaction.ByteString
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import org.http4s.blaze.http.HttpClient

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class EnvironmentHttpClient(settings: NodeConnectionSettings) extends Environment {
  private lazy val mapper = new ObjectMapper() with ScalaObjectMapper
  mapper
    .registerModule(DefaultScalaModule)
    .registerModule(new SimpleModule()
      .addDeserializer(classOf[Option[Transaction]], TransactionDeserializer(mapper, settings.chainId))
      .addDeserializer(classOf[ByteString], ByteStringDeserializer)
      .addDeserializer(classOf[Account], AccountDeserializer(settings.chainId))
      .addDeserializer(classOf[Alias], AliasDeserializer(settings.chainId))
      .addDeserializer(classOf[Order], OrderDeserializer(mapper))
      .addDeserializer(classOf[OrderStatus], OrderStatusDeserializer)
    )

  private lazy val client = HttpClient.pooledHttpClient
  val url = settings.url

  override def height: Long = ???

  override def chainId: Byte = settings.chainId

  override def inputEntity: InputEntity = ???

  override def tthis: Recipient.Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def transactionById(id: Array[Byte]): Option[Tx] =
    get[Option[Transaction]]("/transactions/info/" + Base58.encode(id))

  override def transferTransactionById(id: Array[Byte]): Option[Tx] = ???

  override def transactionHeightById(id: Array[Byte]): Option[Long] = ???

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = ???

  override def lastBlockOpt(): Option[BlockInfo] = ???

  override def blockInfoByHeight(height: Int): Option[BlockInfo] = ???

  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] = ???

  override def resolveAlias(name: String): Either[String, Recipient.Address] = ???

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???

  private def get[R : Manifest](path: String): R =
    Await.result(
      client.GET(url + path)(_.body()).map(b => mapper.readValue[R](b.array)),
      2 seconds
    )
}
