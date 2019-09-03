package com.wavesplatform.lang.v1.repl

import com.softwaremill.sttp._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.repl.global.sttpBackend
import com.wavesplatform.lang.v1.repl.model.{AssetInfo, Block, HeightResponse}
import com.wavesplatform.lang.v1.repl.model.tx.TransferTransaction
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import io.circe.generic.auto._
import io.circe.parser.decode

import scala.concurrent.Await
import scala.concurrent.duration._

case class WebEnvironment(settings: NodeConnectionSettings) extends Environment {
  override def chainId: Byte = settings.chainId
  override def tthis: Recipient.Address = Address(ByteStr.decodeBase58(settings.address).get)


  override def height: Long =
    decode[HeightResponse](get("/blocks/height").get)
      .getOrElse(throw new RuntimeException())
      .height

  override def transferTransactionById(id: Array[Byte]): Option[Tx] = {
    val str = get(s"/transactions/info/${Base58.encode(id)}")
      .map(decode[TransferTransaction])
      .map(_.explicitGet())
    println(str)
    ???
  }

  override def transactionHeightById(id: Array[Byte]): Option[Long] = {
    val str = get(s"/transactions/info/3CzX7VTqSmxMUxNiAD6AoGfQQycz4ECGcS7ERtWRzVMs")
      .map(decode[HeightResponse])
      .map(_.explicitGet())
      .map(_.height)
    println(str)
    ???
  }

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] = {
    val str = get(s"/assets/details/6WfzEZc6nStQgQwzxsKe7kubLdGjYDCz2PhCqZr9PD8q")
      .map(decode[AssetInfo])
      .map(_.explicitGet())
    println(str)
    ???
  }

  override def lastBlockOpt(): Option[BlockInfo] =
    blockInfoByHeight(height.toInt)

  override def blockInfoByHeight(height: Int): Option[BlockInfo] = {
    val str = get(s"/blocks/at/$height")
      .map(decode[Block])
      .map(_.explicitGet())
    println(str)
    ???
  }

  override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any] = {
    val address = addressOrAlias match {
      case Address(bytes)        => bytes.base58
      case Recipient.Alias(name) => resolveAlias(name).explicitGet().bytes.base58
    }
    val str = get(s"/addresses/data/$address/")
  }

  override def resolveAlias(name: String): Either[String, Recipient.Address] = ???

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???

  private val schemaRegex = "^\\w+://.+".r
  private val timeout = 5 seconds

  private def get(path: String): Option[String] = {
    val urlPrefix =
      if (schemaRegex.findFirstMatchIn(settings.url).nonEmpty) ""
      else "http://"
    val url = s"$urlPrefix${settings.url}$path"
    val request = sttp.get(uri"$url").copy(headers = List(("User-Agent", "Chrome")))
    val response = Await.result(request.send(), timeout)
    if (response.code == 404) None
    else Some(response.unsafeBody)
  }

  override def inputEntity: InputEntity                     = ???
  override def transactionById(id: Array[Byte]): Option[Tx] = ???
}
