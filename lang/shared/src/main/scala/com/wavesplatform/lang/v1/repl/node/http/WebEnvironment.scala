package com.wavesplatform.lang.v1.repl.node.http

import cats.implicits._
import cats.{Functor, Id}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.v1.repl.node.http.NodeClient._
import com.wavesplatform.lang.v1.repl.node.http.response.ImplicitMappings
import com.wavesplatform.lang.v1.repl.node.http.response.model.Transaction._
import com.wavesplatform.lang.v1.repl.node.http.response.model._
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import io.circe.{Decoder, HCursor}
import monix.eval.Coeval
import shapeless.Coproduct

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

private[repl] case class WebEnvironment(settings: NodeConnectionSettings) extends Environment[Future] {
  private val client   = NodeClient(settings.normalizedUrl)
  private val mappings = ImplicitMappings(settings.chainId)
  import mappings._

  override implicit def chainId: Byte = settings.chainId
  override def tthis: Environment.Tthis = Coproduct[Environment.Tthis](Address(ByteStr.decodeBase58(settings.address).get))

  override def height: Future[Long] =
    getEntity[Id, HeightResponse, Long]("/blocks/height")

  override def transferTransactionById(id: Array[Byte]): Future[Option[Tx.Transfer]] =
    getEntity[Option, TransferTransaction, Option[Tx.Transfer]](s"/transactions/info/${Base58.encode(id)}?bodyBytes=true").map(_.flatten)

  override def transactionHeightById(id: Array[Byte]): Future[Option[Long]] =
    getEntity[Option, HeightResponse, Option[Long]](s"/transactions/info/${Base58.encode(id)}").map(_.flatten)

  implicit val assetInfoResponseDecoder: Decoder[AssetInfoResponse] = new Decoder[AssetInfoResponse] {
    final def apply(c: HCursor): Decoder.Result[AssetInfoResponse] =
      for {
        assetId <- c.downField("assetId").as[ByteString]
        name <- c.downField("name").as[String]
        description <- c.downField("description").as[String]
        quantity <- c.downField("quantity").as[Long]
        decimals <- c.downField("decimals").as[Int]
        issuer <- c.downField("issuer").as[ByteString]
        issuerPublicKey <- c.downField("issuerPublicKey").as[ByteString]
        reissuable <- c.downField("reissuable").as[Boolean]
        scripted <- c.downField("scripted").as[Boolean]
        minSponsoredAssetFee <- c.downField("minSponsoredAssetFee").as[Option[Long]]
      } yield {
        new AssetInfoResponse(assetId, name, description, quantity, decimals, issuer, issuerPublicKey, reissuable,  scripted, minSponsoredAssetFee)
      }
  }
  override def assetInfoById(id: Array[Byte]): Future[Option[ScriptAssetInfo]] =
    getEntity[Option, AssetInfoResponse, ScriptAssetInfo](s"/assets/details/${Base58.encode(id)}")

  override def lastBlockOpt(): Future[Option[BlockInfo]] =
    height.flatMap(h => blockInfoByHeight(h.toInt))

  implicit val nxtDecoder: Decoder[NxtData] = new Decoder[NxtData] {
    final def apply(c: HCursor): Decoder.Result[NxtData] =
      for {
        bt <- c.downField("base-target").as[Long]
        gs <- c.downField("generation-signature").as[ByteString]
      } yield NxtData(bt, gs)
  }

  implicit val blockInfoResponseDecoder: Decoder[BlockInfoResponse] = new Decoder[BlockInfoResponse] {
    final def apply(c: HCursor): Decoder.Result[BlockInfoResponse] =
      for {
        timestamp <- c.downField("timestamp").as[Long]
        height <- c.downField("height").as[Int]
        nxt <- c.downField("nxt-consensus").as[NxtData]
        generator <- c.downField("generator").as[ByteString]
        generatorPublicKey <- c.downField("generatorPublicKey").as[ByteString]
        vrf <- c.downField("VRF").as[Option[ByteString]]
     } yield BlockInfoResponse(timestamp, height, nxt, generator, generatorPublicKey, vrf)
  }

  override def blockInfoByHeight(height: Int): Future[Option[BlockInfo]] =
    getEntity[Option, BlockInfoResponse, BlockInfo](s"/blocks/at/$height")

  override def data(recipient: Recipient, key: String, dataType: DataType): Future[Option[Any]] =
    for {
      address <- extractAddress(recipient)
      entity  <- getEntity[Option, DataEntry, DataEntry](s"/addresses/data/$address/$key")
      filteredResult = entity.filter(_.`type` == dataType).map(_.value)
    } yield filteredResult

  override def hasData(recipient: Recipient): Future[Boolean] = Future.failed(new Exception("Not implemented"))

  implicit val addressResponseDecoder: Decoder[AddressResponse] = new Decoder[AddressResponse] {
    final def apply(c: HCursor): Decoder.Result[AddressResponse] =
      for {
        address <- c.downField("address").as[ByteString]
      } yield AddressResponse(address)
  }

  override def resolveAlias(name: String): Future[Either[String, Address]] =
    getEntity[Either[String, ?], AddressResponse, Address](s"/alias/by-alias/$name")

  implicit val balanceResponseDecoder: Decoder[BalanceResponse] = new Decoder[BalanceResponse] {
    final def apply(c: HCursor): Decoder.Result[BalanceResponse] =
      for {
        balance <- c.downField("balance").as[Long]
      } yield BalanceResponse(balance)
  }

  override def accountBalanceOf(
      recipient: Recipient,
      assetId: Option[Array[Byte]]
  ): Future[Either[String, Long]] =
    for {
      address <- extractAddress(recipient)
      entity <- getEntity[Either[String, ?], BalanceResponse, Long]((assetId match {
        case Some(assetId) => s"/assets/balance/${address}/${Base58.encode(assetId)}"
        case None          => s"/address/balance/${address}"
      }))
    } yield entity

  override def accountWavesBalanceOf(
      recipient: Recipient
  ): Future[Either[String, Environment.BalanceDetails]] =
    for {
      address <- extractAddress(recipient)
      entity  <- client.get[Either[String, ?], Environment.BalanceDetails](s"/addresses/balance/details/$address")
    } yield entity

  private def extractAddress(addressOrAlias: Recipient): Future[String] =
    addressOrAlias match {
      case Address(bytes) => Future.successful(bytes.toString)
      case Alias(name)    => resolveAlias(name).map(_.explicitGet().bytes.toString)
    }

  override def addressFromString(address: String): Either[String, Address] =
    mappings.addressFromString(address)

  override def inputEntity: InputEntity                             = ???
  override def transactionById(id: Array[Byte]): Future[Option[Tx]] = ???
  override def multiPaymentAllowed: Boolean                         = ???
  override def txId: ByteStr                                        = ???

  override def transferTransactionFromProto(b: Array[Byte]): Future[Option[Tx.Transfer]] = ???

  private def getEntity[F[_]: Functor: ResponseWrapper, A : Decoder, B](url: String)(implicit ev: A => B): Future[F[B]] =
    client.get[F, A](url).map(_.map(ev))

  override def accountScript(addressOrAlias: Recipient): Future[Option[Script]]                                        = ???
  override def callScript(dApp: Address, func: String, args: List[EVALUATED], payments: Seq[(Option[Array[Byte]], Long)], availableComplexity: Int, reentrant: Boolean): Coeval[Future[(Either[ValidationError, EVALUATED], Int)]] = ???
}
