package com.wavesplatform.lang.v1.repl.http

import cats.Id
import cats.implicits._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.v1.repl.http.NodeClient._
import com.wavesplatform.lang.v1.repl.http.response._
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import io.circe.generic.auto._

private[repl] case class WebEnvironment(settings: NodeConnectionSettings) extends Environment {
  private val client = NodeClient(settings.url)
  private val mapper = ResponseMapper(settings.chainId)

  override def chainId: Byte = settings.chainId
  override def tthis: Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def height: Long =
    client.get[Id, HeightResponse]("/blocks/height")
      .height

  override def transferTransactionById(id: Array[Byte]): Option[Tx] =
    client.get[Option, TransferTransaction](s"/transactions/info/${Base58.encode(id)}")
      .map(mapper.toRideModel)

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    client.get[Option, HeightResponse](s"/transactions/info/${Base58.encode(id)}")
      .map(_.height)

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] =
    client.get[Option, AssetInfoResponse](s"/assets/details/${Base58.encode(id)}")
      .map(mapper.toRideModel)

  override def lastBlockOpt(): Option[BlockInfo] =
    blockInfoByHeight(height.toInt)

  override def blockInfoByHeight(height: Int): Option[BlockInfo] =
    client.get[Option, BlockInfoResponse](s"/blocks/at/$height")
      .map(mapper.toRideModel)

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] =
    client.get[Option, DataEntry](s"/addresses/data/${address(recipient)}/$key")
      .filter(_.`type` == dataType)
      .map(_.value)

  override def resolveAlias(name: String): Either[String, Address] =
    client.get[Either[String, ?], AddressResponse](s"/alias/by-alias/$name")
      .map(a => Address(a.address.byteStr))

  override def accountBalanceOf(recipient: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
    client.get[Either[String, ?], BalanceResponse](s"/addresses/balance/${address(recipient)}")
      .map(_.balance)

  override def inputEntity: InputEntity                     = ???
  override def transactionById(id: Array[Byte]): Option[Tx] = ???

  private def address(addressOrAlias: Recipient) =
    addressOrAlias match {
      case Address(bytes) => bytes.base58
      case Alias(name)    => resolveAlias(name).explicitGet().bytes.base58
    }
}
