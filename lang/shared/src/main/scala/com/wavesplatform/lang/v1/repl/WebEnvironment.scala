package com.wavesplatform.lang.v1.repl

import java.net.URI

import com.softwaremill.sttp._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.repl.model.{HeightResponse, Transaction}
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import upickle.default._
import Global.sttpBackend

import scala.concurrent.Await
import scala.concurrent.duration._

case class WebEnvironment(settings: NodeConnectionSettings) extends Environment {
  private val timeout = 5 seconds

  override def height: Long = read[HeightResponse](get("/blocks/height")).height

  override def chainId: Byte = settings.chainId

  override def inputEntity: InputEntity = ???

  override def tthis: Recipient.Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def transactionById(id: Array[Byte]): Option[Tx] = {
    val r = read[Option[Transaction]](get("/transactions/info/sfsdfs"))
    println(r)
    ???
  }

  override def transferTransactionById(id: Array[Byte]): Option[Tx] = ???

  override def transactionHeightById(id: Array[Byte]): Option[Long] = ???

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = ???

  override def lastBlockOpt(): Option[BlockInfo] = ???

  override def blockInfoByHeight(height: Int): Option[BlockInfo] = ???

  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] = ???

  override def resolveAlias(name: String): Either[String, Recipient.Address] = ???

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???

  private val schemaRegex = "^\\w+://.+".r

  private def get(path: String): String = {
    val urlPrefix =
      if (schemaRegex.findFirstMatchIn(settings.url).nonEmpty) ""
      else "http://"
    val url = s"$urlPrefix${settings.url}$path"
    val request = sttp.get(uri"$url").copy(headers = List(("User-Agent", "Chrome")))
    Await.result(request.send(), timeout).unsafeBody
  }
}
