package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import cats.instances.list._
import cats.instances.try_._
import cats.syntax.traverse._
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.CommonTransactionsApi
import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.block.Block
import com.wavesplatform.block.merkle.Merkle.TransactionProof
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.network.UtxPoolSynchronizer
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
    with AuthRoute {
  import TransactionsApiRoute._

  private[this] val commonApi = new CommonTransactionsApi(blockchain, utx, wallet, utxPoolSynchronizer.publish)

  override lazy val route: Route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ sign ~ calculateFee ~ signedBroadcast ~ merkleProof
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

  def merkleProof: Route = path("merkleProof")(merkleProofGet ~ merkleProofPost)

  @Path("/merkleProof")
  @ApiOperation(value = "Transaction's merkle proof", notes = "Transaction's merkle proof", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "id",
        required = true,
        paramType = "query",
        dataType = "string",
        value = "Transaction IDs"
      )
    )
  )
  def merkleProofGet: Route = (get & parameters('id.*))(ids => complete(merkleProof(ids.toList)))

  @Path("/merkleProof")
  @ApiOperation(value = "Transaction's merkle proof", notes = "Transaction's merkle proof", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "json",
        required = true,
        paramType = "body",
        dataType = "string",
        value = "Transaction IDs",
        example = """{"ids": ["some1", "some2"]}"""
      )
    )
  )
  def merkleProofPost: Route =
    jsonPost[JsObject](
      jsv =>
        (jsv \ "ids").validate[List[String]] match {
          case JsSuccess(ids, _) => merkleProof(ids)
          case JsError(err)      => WrongJson(errors = err)
        }
    )

  private def merkleProof(encodedIds: List[String]): ToResponseMarshallable =
    encodedIds.traverse(ByteStr.decodeBase58) match {
      case Success(txIds) =>
        commonApi.transactionProofs(txIds) match {
          case Nil    => CustomValidationError(s"transactions do not exists or block version < ${Block.ProtoBlockVersion}")
          case proofs => proofs
        }
      case _ => InvalidSignature
    }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    import com.wavesplatform.transaction.lease.LeaseTransaction
    tx match {
      case lease: LeaseTransaction =>
        import com.wavesplatform.api.http.TransactionsApiRoute.LeaseStatus._
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

object TransactionsApiRoute {
  object LeaseStatus {
    val Active   = "active"
    val Canceled = "canceled"
  }

  implicit val transactionProofWrites: Writes[TransactionProof] = Writes { mi =>
    def proofBytes(levels: Seq[Array[Byte]]): List[String] =
      (levels foldRight List.empty[String]) { case (d, acc) => s"${Base64.Prefix}${Base64.encode(d)}" :: acc }

    Json.obj(
      "id"               -> mi.id.toString,
      "transactionIndex" -> mi.transactionIndex,
      "merkleProof"      -> proofBytes(mi.digests)
    )
  }

  implicit val transactionProofReads: Reads[TransactionProof] = Reads { jsv =>
    for {
      encoded          <- (jsv \ "id").validate[String]
      id               <- ByteStr.decodeBase58(encoded).fold(_ => JsError(InvalidSignature.message), JsSuccess(_))
      transactionIndex <- (jsv \ "transactionIndex").validate[Int]
      merkleProof      <- (jsv \ "merkleProof").validate[List[String]].map(_.map(Base64.decode))
    } yield TransactionProof(id, transactionIndex, merkleProof)
  }
}
