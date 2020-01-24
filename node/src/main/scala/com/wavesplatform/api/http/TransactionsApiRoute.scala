package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import cats.implicits._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.protobuf.api.TransactionsByIdRequest
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.swagger.annotations._
import javax.ws.rs.Path
import monix.execution.Scheduler
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Success

@Path("/transactions")
@Api(value = "/transactions")
case class TransactionsApiRoute(
    settings: RestAPISettings,
    wallet: Wallet,
    blockchain: Blockchain,
    utx: UtxPool,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    time: Time
) extends ApiRoute
    with BroadcastRoute
    with AuthRoute
    with AutoParamsDirective {

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, utxPoolSynchronizer.publish)

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ status ~ sign ~ calculateFee ~ signedBroadcast
    }

  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(
    value = "List of transactions by address",
    notes = "Get list of transactions where specified address has been involved",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "limit",
        value = "Number of transactions to be returned",
        required = true,
        dataType = "integer",
        paramType = "path"
      ),
      new ApiImplicitParam(name = "after", value = "Id of transaction to paginate after", required = false, dataType = "string", paramType = "query")
    )
  )
  def addressLimit: Route = {
    (get & path("address" / Segment / "limit" / IntNumber) & parameter('after.?)) { (address, limit, maybeAfter) =>
      extractScheduler(implicit sc => complete(transactionsByAddress(address, limit, maybeAfter)))
    }
  }

  @Path("/info/{id}")
  @ApiOperation(value = "Transaction info", notes = "Get a transaction by its ID", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction ID", required = true, dataType = "string", paramType = "path")
    )
  )
  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            commonApi.transactionById(id) match {
              case Some((h, tx)) => complete(txToExtendedJson(tx) + ("height" -> JsNumber(h)))
              case None          => complete(ApiError.TransactionDoesNotExist)
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/status")
  @ApiOperation(value = "Transaction status", notes = "Get a transaction status by its ID", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction ID", required = true, dataType = "string", paramType = "query", allowMultiple = true)
    )
  )
  def status: Route = path("status") {
    protobufEntity(TransactionsByIdRequest) { request =>
      if (request.ids.length > settings.transactionsByAddressLimit)
        complete(TooBigArrayAllocation)
      else {
        request.ids.map(id => ByteStr.decodeBase58(id).toEither.leftMap(_ => id)).toList.separate match {
          case (Nil, Nil)  => complete(CustomValidationError("Empty request"))
          case (Nil, ids) =>
            val results = ids.toSet.map { id: ByteStr =>
              val statusJson = blockchain.transactionInfo(id) match {
                case Some((height, _)) =>
                  Json.obj("status" -> "confirmed", "height" -> height, "confirmations" -> (blockchain.height - height).max(0))

                case None =>
                  utx.transactionById(id) match {
                    case Some(_) => Json.obj("status" -> "unconfirmed")
                    case None    => Json.obj("status" -> "not_found")
                  }
              }
              id -> (statusJson ++ Json.obj("id" -> id.toString))
            }.toMap
            complete(ids.map(id => results(id)))
          case (errors, _) => complete(InvalidIds(errors))
        }
      }
    }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed transactions", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(commonApi.unconfirmedTransactions().map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @ApiOperation(
    value = "Number of unconfirmed transactions",
    notes = "Get the number of unconfirmed transactions in the UTX pool",
    httpMethod = "GET"
  )
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utx.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @ApiOperation(value = "Unconfirmed transaction info", notes = "Get an unconfirmed transaction by its ID", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction ID", required = true, dataType = "string", paramType = "path")
    )
  )
  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            commonApi.unconfirmedTransactionById(id) match {
              case Some(tx) =>
                complete(txToExtendedJson(tx))
              case None =>
                complete(ApiError.TransactionDoesNotExist)
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/calculateFee")
  @ApiOperation(value = "Calculate transaction fee", notes = "Calculates minimal fee for a transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json", required = true, dataType = "string", paramType = "body", value = "Transaction data including type")
    )
  )
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

  @Path("/sign")
  @ApiOperation(value = "Sign a transaction", notes = "Sign a transaction with the sender's private key", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "json",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Transaction data including <a href='transaction-types.html'>type</a>"
      )
    )
  )
  def sign: Route = (pathPrefix("sign") & withAuth) {
    pathEndOrSingleSlash(jsonPost[JsObject] { jsv =>
      TransactionFactory.parseRequestAndSign(wallet, (jsv \ "sender").as[String], time, jsv)
    }) ~ signWithSigner
  }

  @Path("/sign/{signerAddress}")
  @ApiOperation(
    value = "Sign a transaction with a non-default private key",
    notes = "Sign a transaction with the private key corresponding to the given address",
    httpMethod = "POST"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "signerAddress", value = "Wallet address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(
        name = "json",
        required = true,
        dataType = "string",
        paramType = "body",
        value = "Transaction data including <a href='transaction-types.html'>type</a>"
      )
    )
  )
  def signWithSigner: Route = pathPrefix(Segment) { signerAddress =>
    jsonPost[JsObject](TransactionFactory.parseRequestAndSign(wallet, signerAddress, time, _))
  }

  @Path("/broadcast")
  @ApiOperation(value = "Broadcast a signed transaction", notes = "Broadcast a signed transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "json",
        required = true,
        paramType = "body",
        dataType = "string",
        value = "Transaction data including <a href='transaction-types.html'>type</a> and signature/proofs"
      )
    )
  )
  def signedBroadcast: Route = path("broadcast")(broadcast[JsValue](TransactionFactory.fromSignedRequest))

  private def txToExtendedJson(tx: Transaction): JsObject = {
    import com.wavesplatform.transaction.lease.LeaseTransaction
    tx match {
      case lease: LeaseTransaction =>
        import com.wavesplatform.transaction.lease.LeaseTransaction.Status._
        lease.json() ++ Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) Active else Canceled))

      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> blockchain.transactionInfo(leaseCancel.leaseId).map(_._2.json()).getOrElse[JsValue](JsNull))

      case t => t.json()
    }
  }

  def transactionsByAddress(addressParam: String, limitParam: Int, maybeAfterParam: Option[String])(
      implicit sc: Scheduler
  ): Either[ApiError, Future[JsArray]] = {
    def createTransactionsJsonArray(address: Address, limit: Int, fromId: Option[ByteStr]): Future[JsArray] = {
      lazy val addressesCached = concurrent.blocking(blockchain.aliasesOfAddress(address).toVector :+ address).toSet

      /**
        * Produces compact representation for large transactions by stripping unnecessary data.
        * Currently implemented for MassTransfer transaction only.
        */
      def txToCompactJson(address: Address, tx: Transaction): JsObject = {
        import com.wavesplatform.transaction.transfer._
        tx match {
          case mtt: MassTransferTransaction if mtt.sender.toAddress != address => mtt.compactJson(addressesCached)
          case _                                                               => txToExtendedJson(tx)
        }
      }

      commonApi
        .transactionsByAddress(address, fromId)
        .take(limit)
        .toListL
        .map(txs => Json.arr(JsArray(txs.map { case (height, tx) => txToCompactJson(address, tx) + ("height" -> JsNumber(height)) })))
        .runToFuture
    }

    for {
      address <- Address.fromString(addressParam).left.map(ApiError.fromValidationError)
      limit   <- Either.cond(limitParam <= settings.transactionsByAddressLimit, limitParam, TooBigArrayAllocation)
      maybeAfter <- maybeAfterParam match {
        case Some(v) =>
          ByteStr
            .decodeBase58(v)
            .fold(
              _ => Left(CustomValidationError(s"Unable to decode transaction id $v")),
              id => Right(Some(id))
            )
        case None => Right(None)
      }
    } yield createTransactionsJsonArray(address, limit, maybeAfter)
  }
}
