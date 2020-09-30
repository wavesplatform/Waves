package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.instances.either._
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.alternative._
import cats.syntax.either._
import cats.syntax.traverse._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.utils.Time
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Success

case class TransactionsApiRoute(
    settings: RestAPISettings,
    commonApi: CommonTransactionsApi,
    wallet: Wallet,
    blockchain: Blockchain,
    utxPoolSize: () => Int,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    time: Time
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute {
  import TransactionsApiRoute._

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
    }

  def addressLimit: Route = {
    (get & path("address" / AddrSegment / "limit" / IntNumber) & parameter("after".?)) { (address, limit, maybeAfter) =>
      val after =
        maybeAfter.map(s => ByteStr.decodeBase58(s).getOrElse(throw ApiException(CustomValidationError(s"Unable to decode transaction id $s"))))
      if (limit > settings.transactionsByAddressLimit) throw ApiException(TooBigArrayAllocation)
      extractScheduler { implicit sc =>
        complete(transactionsByAddress(address, limit, after).map(txs => List(txs)))
      }
    }
  }

  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidTransactionId("Transaction ID was not specified"))
    } ~ path(TransactionId) { id =>
      commonApi.transactionById(id) match {
        case Some((h, either, succeeded)) =>
          complete(txToExtendedJson(either.fold(identity, _._1)) ++ applicationStatus(isBlockV5(h), succeeded) + ("height" -> JsNumber(h)))
        case None => complete(ApiError.TransactionDoesNotExist)
      }
    }
  }

  private def loadTransactionStatus(id: ByteStr): JsObject = {
    import Status._
    val statusJson = blockchain.transactionInfo(id) match {
      case Some((height, _, succeeded)) =>
        Json.obj(
          "status"        -> Confirmed,
          "height"        -> height,
          "confirmations" -> (blockchain.height - height).max(0)
        ) ++ applicationStatus(isBlockV5(height), succeeded)
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
      complete(JsArray(commonApi.unconfirmedTransactions.map(txToExtendedJson)))
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
            complete(txToExtendedJson(tx))
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
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, address.stringRepr, time, _))
  }

  def signedBroadcast: Route = path("broadcast")(broadcast[JsValue](TransactionFactory.fromSignedRequest))

  def merkleProof: Route = path("merkleProof") {
    (get & parameters("id".as[String].*))(ids => complete(merkleProof(ids.toList.reverse))) ~
      jsonPost[JsObject](
        jsv =>
          (jsv \ "ids").validate[List[String]] match {
            case JsSuccess(ids, _) => merkleProof(ids)
            case JsError(err)      => WrongJson(errors = err.toSeq)
          }
      )
  }

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.traverse(ByteStr.decodeBase58) match {
      case Success(txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exist or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case _ => InvalidSignature
    }

  private def txToExtendedJson(tx: Transaction): JsObject = tx match {
    case lease: LeaseTransaction =>
      import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus._
      lease.json() ++ Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) Active else Canceled))

    case leaseCancel: LeaseCancelTransaction =>
      leaseCancel.json() ++ Json.obj("lease" -> blockchain.transactionInfo(leaseCancel.leaseId).map(_._2.json()).getOrElse[JsValue](JsNull))

    case t => t.json()
  }

  def transactionsByAddress(address: Address, limitParam: Int, maybeAfter: Option[ByteStr])(implicit sc: Scheduler): Future[List[JsObject]] = {
    val aliasesOfAddress =
      commonApi
        .aliasesOfAddress(address)
        .collect { case (_, cat) => cat.alias }
        .toListL
        .map(aliases => (address :: aliases).toSet)
        .memoize

    /**
      * Produces compact representation for large transactions by stripping unnecessary data.
      * Currently implemented for MassTransfer transaction only.
      */
    def txToCompactJson(address: Address, tx: Transaction): Task[JsObject] = {
      import com.wavesplatform.transaction.transfer._
      tx match {
        case mtt: MassTransferTransaction if mtt.sender.toAddress != address => aliasesOfAddress.map(mtt.compactJson)
        case _                                                               => Task.now(txToExtendedJson(tx))
      }
    }

    commonApi
      .transactionsByAddress(address, None, Set.empty, maybeAfter)
      .take(limitParam)
      .mapEval {
        case (height, tx, succeeded) =>
          txToCompactJson(address, tx).map(_ ++ applicationStatus(isBlockV5(height), succeeded) + ("height" -> JsNumber(height)))
      }
      .toListL
      .runToFuture
  }

  private def isBlockV5(height: Int): Boolean = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, height)
}

object TransactionsApiRoute {
  object LeaseStatus {
    val Active   = "active"
    val Canceled = "canceled"
  }

  object Status {
    val Confirmed   = "confirmed"
    val Unconfirmed = "unconfirmed"
    val NotFound    = "not_found"
  }

  object ApplicationStatus {
    val Succeeded             = "succeeded"
    val ScriptExecutionFailed = "script_execution_failed"
  }

  def applicationStatus(isBlockV5: Boolean, succeeded: Boolean): JsObject =
    if (isBlockV5)
      JsObject(Map("applicationStatus" -> JsString(if (succeeded) ApplicationStatus.Succeeded else ApplicationStatus.ScriptExecutionFailed)))
    else
      JsObject.empty

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
}
