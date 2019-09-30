package com.wavesplatform.lang.v1.repl.node.http

import cats.implicits._
import cats.{Functor, Id}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.v1.repl.node.http.NodeClient._
import com.wavesplatform.lang.v1.repl.node.http.response.ImplicitMappings
import com.wavesplatform.lang.v1.repl.node.http.response.model._
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import io.circe.Decoder
import io.circe.generic.auto._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[repl] case class WebEnvironment(settings: NodeConnectionSettings) extends Environment[Future] {
  private val client = NodeClient(settings.normalizedUrl)
  private val mappings = ImplicitMappings(settings.chainId)
  import mappings._

  override implicit def chainId: Byte = settings.chainId
  override def tthis: Address = Address(ByteStr.decodeBase58(settings.address).get)

  override def height: Future[Long] =
    getEntity[Id, HeightResponse, Long]("/blocks/height")

  override def transferTransactionById(id: Array[Byte]): Future[Option[Tx]] =
    getEntity[Option, TransferTransaction, Tx](s"/transactions/info/${Base58.encode(id)}")

  override def transactionHeightById(id: Array[Byte]): Future[Option[Long]] =
    getEntity[Option, HeightResponse, Long](s"/transactions/info/${Base58.encode(id)}")

  override def assetInfoById(id: Array[Byte]): Future[Option[ScriptAssetInfo]] =
    getEntity[Option, AssetInfoResponse, ScriptAssetInfo](s"/assets/details/${Base58.encode(id)}")

  override def lastBlockOpt(): Future[Option[BlockInfo]] =
    height.flatMap(h => blockInfoByHeight(h.toInt))

  override def blockInfoByHeight(height: Int): Future[Option[BlockInfo]] =
    getEntity[Option, BlockInfoResponse, BlockInfo](s"/blocks/at/$height")

  override def data(recipient: Recipient, key: String, dataType: DataType): Future[Option[Any]] =
    for {
      address <- extractAddress(recipient)
      entity  <- getEntity[Option, DataEntry, DataEntry](s"/addresses/data/$address/$key")
      filteredResult = entity.filter(_.`type` == dataType).map(_.value)
    } yield filteredResult

  override def resolveAlias(name: String): Future[Either[String, Address]] =
    getEntity[Either[String, ?], AddressResponse, Address](s"/alias/by-alias/$name")

  override def accountBalanceOf(
    recipient: Recipient,
    assetId:   Option[Array[Byte]]
  ): Future[Either[String, Long]] =
    for {
     address <- extractAddress(recipient)
     entity  <- getEntity[Either[String, ?], BalanceResponse, Long](s"/addresses/balance/$address")
    } yield entity

  override def inputEntity: InputEntity                             = ???
  override def transactionById(id: Array[Byte]): Future[Option[Tx]] = ???

  private def extractAddress(addressOrAlias: Recipient): Future[String] =
    addressOrAlias match {
      case Address(bytes) => Future.successful(bytes.base58)
      case Alias(name)    => resolveAlias(name).map(_.explicitGet().bytes.base58)
    }

  private def getEntity[F[_] : Functor : ResponseWrapper, A <% B : Decoder, B](url: String): Future[F[B]] =
    client.get[F, A](url).map(_.map(r => r))
}
