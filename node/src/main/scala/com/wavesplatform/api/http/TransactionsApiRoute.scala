package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.instances.either.*
import cats.instances.list.*
import cats.syntax.alternative.*
import cats.syntax.either.*
import cats.syntax.traverse.*
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.common.{CommonTransactionsApi, TransactionMeta}
import com.wavesplatform.api.http.ApiError.*
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.network.TransactionPublisher
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.transfer.MassTransferTransaction
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.reactive.Observable
import play.api.libs.json.*

case class TransactionsApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    blockchain: Blockchain,
    compositeBlockchain: () => Blockchain,
    utxPoolSize: () => Int,
    transactionPublisher: TransactionPublisher,
    time: Time,
    routeTimeout: RouteTimeout
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {
  import TransactionsApiRoute.*

  private[this] val serializer                     = TransactionJsonSerializer(blockchain, commonApi)
  private[this] implicit val transactionMetaWrites = OWrites[TransactionMeta](serializer.transactionWithMetaJson)

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressWithLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
    }

  def addressWithLimit: Route = {
    (get & path("address" / AddrSegment / "limit" / IntNumber) & parameter("after".?)) { (address, limit, maybeAfter) =>
      val after =
        maybeAfter.map(s => ByteStr.decodeBase58(s).getOrElse(throw ApiException(CustomValidationError(s"Unable to decode transaction id $s"))))
      if (limit > settings.transactionsByAddressLimit) throw ApiException(TooBigArrayAllocation)

      val blockV5Activation  = blockchain.activatedFeatures.get(BlockchainFeatures.BlockV5.id)
      val improvedSerializer = serializer.copy(blockchain = compositeBlockchain())

      routeTimeout.executeFromObservable {
        transactionsByAddress(address, limit, after) // Double list - [ [tx1, tx2, ...] ]
      }(jacksonStreamMarshaller("[[", ",", "]]")(improvedSerializer.txMetaJsonSerializer(address, h => blockV5Activation.exists(v5h => v5h <= h), _)))
    }
  }

  private[this] def readTransactionMeta(id: String): Either[ApiError, TransactionMeta] =
    for {
      id   <- ByteStr.decodeBase58(id).toEither.leftMap(err => CustomValidationError(err.toString))
      meta <- commonApi.transactionById(id).toRight(ApiError.TransactionDoesNotExist)
    } yield meta

  def info: Route = pathPrefix("info") {
    (get & path(TransactionId)) { id =>
      complete(commonApi.transactionById(id).toRight(ApiError.TransactionDoesNotExist))
    } ~ (pathEndOrSingleSlash & anyParam("id", limit = settings.transactionsByAddressLimit)) { ids =>
      val result = for {
        _    <- Either.cond(ids.nonEmpty, (), InvalidTransactionId("Transaction ID was not specified"))
        meta <- ids.map(readTransactionMeta).toList.sequence
      } yield meta

      complete(result)
    }
  }

  private[this] def loadTransactionStatus(id: ByteStr): JsObject = {
    import Status.*
    val statusJson = blockchain.transactionInfo(id) match {
      case Some((tm, _)) =>
        Json.obj(
          "status"        -> Confirmed,
          "height"        -> JsNumber(tm.height),
          "confirmations" -> (blockchain.height - tm.height).max(0)
        ) ++ serializer.metaJson(tm)
      case None =>
        commonApi.unconfirmedTransactionById(id) match {
          case Some(_) => Json.obj("status" -> Unconfirmed)
          case None    => Json.obj("status" -> NotFound)
        }
    }
    statusJson ++ Json.obj("id" -> id.toString)
  }

  def status: Route = pathPrefix("status") {
    path(TransactionId) { id =>
      complete(loadTransactionStatus(id))
    } ~ pathEndOrSingleSlash {
      anyParam("id").filter(_.nonEmpty) { ids =>
        if (ids.toSeq.length > settings.transactionsByAddressLimit)
          complete(TooBigArrayAllocation)
        else {
          ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).toList.separate match {
            case (Nil, ids) =>
              val results = ids.toSet.map((id: ByteStr) => id -> loadTransactionStatus(id)).toMap
              complete(ids.map(id => results(id)))
            case (errors, _) => complete(InvalidIds(errors))
          }
        }
      } ~ pathEndOrSingleSlash {
        complete(CustomValidationError("Empty request"))
      }
    }
  }

  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(commonApi.unconfirmedTransactions.map(serializer.unconfirmedTxExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utxPoolSize())))
  }

  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(TransactionId) { id =>
        commonApi.unconfirmedTransactionById(id) match {
          case Some(tx) =>
            complete(serializer.unconfirmedTxExtendedJson(tx))
          case None =>
            complete(ApiError.TransactionDoesNotExist)
        }
      }
  }

  def calculateFee: Route =
    path("calculateFee")(jsonPost[JsObject] { jsv =>
      val senderPk = (jsv \ "senderPublicKey").as[String]
      // Just for converting the request to the transaction
      val enrichedJsv = jsv ++ Json.obj(
        "fee"    -> 1234567,
        "sender" -> senderPk
      )

      createTransaction(senderPk, enrichedJsv) { tx =>
        commonApi
          .calculateFee(tx)
          .map { case (assetId, assetAmount, _) => Json.obj("feeAssetId" -> assetId, "feeAmount" -> assetAmount) }
      }
    })

  def sign: Route = (pathPrefix("sign") & withAuth) {
    pathEndOrSingleSlash(jsonPost[JsObject] { jsv =>
      TransactionFactory.parseRequestAndSign(wallet, (jsv \ "sender").as[String], time, jsv)
    }) ~ signWithSigner
  }

  def signWithSigner: Route = path(AddrSegment) { address =>
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, address.toString, time, _))
  }

  def signedBroadcast: Route = path("broadcast") {
    broadcast[JsValue](TransactionFactory.fromSignedRequest)
  }

  def merkleProof: Route = path("merkleProof") {
    anyParam("id", limit = settings.transactionsByAddressLimit) { ids =>
      val result = Either
        .cond(ids.nonEmpty, (), InvalidTransactionId("Transaction ID was not specified"))
        .map(_ => merkleProof(ids.toList))
      complete(result)
    }
  }

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).separate match {
      case (Nil, txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case (errors, _) => InvalidIds(errors)
    }

  def transactionsByAddress(address: Address, limitParam: Int, maybeAfter: Option[ByteStr]): Observable[TxMetaEnriched] = {
    val aliasesOfAddress: Task[Set[Alias]] =
      commonApi
        .aliasesOfAddress(address)
        .collect { case (_, cat) => cat.alias }
        .toListL
        .map(aliases => aliases.toSet)
        .memoize

    def txMetaEnriched(address: Address, meta: TransactionMeta): Task[TxMetaEnriched] =
      meta.transaction match {
        case mtt: MassTransferTransaction if mtt.sender.toAddress != address =>
          val aliasExists = mtt.transfers.exists(pt =>
            pt.address match {
              case _: Address => false
              case _: Alias   => true
            }
          )

          if (aliasExists) {
            aliasesOfAddress.map(aliases => TxMetaEnriched(meta, Some(aliases)))
          } else {
            Task.now(TxMetaEnriched(meta))
          }
        case _ => Task.now(TxMetaEnriched(meta))
      }

    commonApi
      .transactionsByAddress(address, None, Set.empty, maybeAfter)
      .take(limitParam)
      .mapEval(txMetaEnriched(address, _))
  }
}

object TransactionsApiRoute {
  type LeaseStatus = LeaseStatus.Value

  // noinspection TypeAnnotation
  object LeaseStatus extends Enumeration {
    val active   = Value(1)
    val canceled = Value(0)

    def apply(bool: Boolean): LeaseStatus = if (bool) active else canceled
  }

  object Status {
    val Confirmed   = "confirmed"
    val Unconfirmed = "unconfirmed"
    val NotFound    = "not_found"
  }

  object ApplicationStatus {
    val Succeeded             = "succeeded"
    val ScriptExecutionFailed = "script_execution_failed"
    val Elided                = "elided"
  }

  implicit val transactionProofWrites: Writes[TransactionProof] = Writes { mi =>
    Json.obj(
      "id"               -> mi.id.toString,
      "transactionIndex" -> mi.transactionIndex,
      "merkleProof"      -> mi.digests.map(d => s"${Base58.encode(d)}")
    )
  }

  implicit val transactionProofReads: Reads[TransactionProof] = Reads { jsv =>
    for {
      encoded          <- (jsv \ "id").validate[String]
      id               <- ByteStr.decodeBase58(encoded).fold(_ => JsError(InvalidSignature.message), JsSuccess(_))
      transactionIndex <- (jsv \ "transactionIndex").validate[Int]
      merkleProof      <- (jsv \ "merkleProof").validate[List[String]].map(_.map(Base58.decode))
    } yield TransactionProof(id, transactionIndex, merkleProof)
  }

  case class TxMetaEnriched(meta: TransactionMeta, aliases: Option[Set[Alias]] = None)
}
