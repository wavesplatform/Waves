package com.wavesplatform.api.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.wavesplatform.account.Address
import com.wavesplatform.api.http.DataRequest._
import com.wavesplatform.api.http.alias.{CreateAliasV1Request, CreateAliasV2Request}
import com.wavesplatform.api.http.assets.SponsorFeeRequest._
import com.wavesplatform.api.http.assets._
import com.wavesplatform.api.http.leasing._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.http.BroadcastRoute
import com.wavesplatform.settings.{FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._

import scala.util.Success

@Path("/transactions")
@Api(value = "/transactions")
case class TransactionsApiRoute(settings: RestAPISettings,
                                functionalitySettings: FunctionalitySettings,
                                wallet: Wallet,
                                blockchain: Blockchain,
                                utx: UtxPool,
                                allChannels: ChannelGroup,
                                time: Time)
    extends ApiRoute
    with BroadcastRoute
    with CommonApiFunctions {

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ sign ~ calculateFee ~ broadcast
    }

  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "List of transactions by address",
                notes = "Get list of transactions where specified address has been involved",
                httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "limit",
                           value = "Number of transactions to be returned",
                           required = true,
                           dataType = "integer",
                           paramType = "path"),
      new ApiImplicitParam(name = "after", value = "Id of transaction to paginate after", required = false, dataType = "string", paramType = "query")
    ))
  def addressLimit: Route = {
    (get & path("address" / Segment / "limit" / IntNumber) & parameter('after.?)) { (address, limit, maybeAfter) =>
      complete(transactionsByAddress(address, limit, maybeAfter))
    }
  }

  @Path("/info/{id}")
  @ApiOperation(value = "Transaction info", notes = "Get a transaction by its ID", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction ID", required = true, dataType = "string", paramType = "path")
    ))
  def info: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            blockchain.transactionInfo(id) match {
              case Some((h, tx)) => complete(txToExtendedJson(tx) + ("height" -> JsNumber(h)))
              case None          => complete(StatusCodes.NotFound             -> Json.obj("status" -> "error", "details" -> "Transaction is not in blockchain"))
            }
          case _ => complete(InvalidSignature)
        }
      }
  }

  @Path("/unconfirmed")
  @ApiOperation(value = "Unconfirmed transactions", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(utx.all.map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @ApiOperation(value = "Number of unconfirmed transactions",
                notes = "Get the number of unconfirmed transactions in the UTX pool",
                httpMethod = "GET")
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utx.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @ApiOperation(value = "Unconfirmed transaction info", notes = "Get an unconfirmed transaction by its ID", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction ID", required = true, dataType = "string", paramType = "path")
    ))
  def utxTransactionInfo: Route = (pathPrefix("info") & get) {
    pathEndOrSingleSlash {
      complete(InvalidSignature)
    } ~
      path(Segment) { encoded =>
        ByteStr.decodeBase58(encoded) match {
          case Success(id) =>
            utx.transactionById(id) match {
              case Some(tx) =>
                complete(txToExtendedJson(tx))
              case None =>
                complete(StatusCodes.NotFound -> Json.obj("status" -> "error", "details" -> "Transaction is not in UTX"))
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
    ))
  def calculateFee: Route = (pathPrefix("calculateFee") & post) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          val senderPk = (jsv \ "senderPublicKey").as[String]
          // Just for converting the request to the transaction
          val enrichedJsv = jsv ++ Json.obj(
            "fee"    -> 1234567,
            "sender" -> senderPk
          )

          createTransaction(senderPk, enrichedJsv) { tx =>
            CommonValidation.getMinFee(blockchain, functionalitySettings, blockchain.height, tx).map {
              case (assetId, assetAmount, wavesAmount) =>
                Json.obj(
                  "feeAssetId" -> assetId,
                  "feeAmount"  -> assetAmount
                )
            }
          }
        }
      }
    }
  }

  @Path("/sign")
  @ApiOperation(value = "Sign a transaction", notes = "Sign a transaction with the sender's private key", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including <a href='transaction-types.html'>type</a>")
    ))
  def sign: Route = (pathPrefix("sign") & post & withAuth) {
    pathEndOrSingleSlash {
      handleExceptions(jsonExceptionHandler) {
        json[JsObject] { jsv =>
          signTransaction((jsv \ "sender").as[String], jsv)
        }
      }
    } ~ signWithSigner
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
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including <a href='transaction-types.html'>type</a>")
    ))
  def signWithSigner: Route = pathPrefix(Segment) { signerAddress =>
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        signTransaction(signerAddress, jsv)
      }
    }
  }

  private def signTransaction(signerAddress: String, jsv: JsObject): ToResponseMarshallable = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => WrongJson(None, errors)
      case JsSuccess(value, _) =>
        val version = value getOrElse (1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        (TransactionParsers.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            x match {
              case IssueTransactionV1       => TransactionFactory.issueAssetV1(txJson.as[IssueV1Request], wallet, signerAddress, time)
              case IssueTransactionV2       => TransactionFactory.issueAssetV2(txJson.as[IssueV2Request], wallet, signerAddress, time)
              case TransferTransactionV1    => TransactionFactory.transferAssetV1(txJson.as[TransferV1Request], wallet, signerAddress, time)
              case TransferTransactionV2    => TransactionFactory.transferAssetV2(txJson.as[TransferV2Request], wallet, signerAddress, time)
              case ReissueTransactionV1     => TransactionFactory.reissueAssetV1(txJson.as[ReissueV1Request], wallet, signerAddress, time)
              case ReissueTransactionV2     => TransactionFactory.reissueAssetV2(txJson.as[ReissueV2Request], wallet, signerAddress, time)
              case BurnTransactionV1        => TransactionFactory.burnAssetV1(txJson.as[BurnV1Request], wallet, signerAddress, time)
              case BurnTransactionV2        => TransactionFactory.burnAssetV2(txJson.as[BurnV2Request], wallet, signerAddress, time)
              case MassTransferTransaction  => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], wallet, signerAddress, time)
              case LeaseTransactionV1       => TransactionFactory.leaseV1(txJson.as[LeaseV1Request], wallet, signerAddress, time)
              case LeaseTransactionV2       => TransactionFactory.leaseV2(txJson.as[LeaseV2Request], wallet, signerAddress, time)
              case LeaseCancelTransactionV1 => TransactionFactory.leaseCancelV1(txJson.as[LeaseCancelV1Request], wallet, signerAddress, time)
              case LeaseCancelTransactionV2 => TransactionFactory.leaseCancelV2(txJson.as[LeaseCancelV2Request], wallet, signerAddress, time)
              case CreateAliasTransactionV1 => TransactionFactory.aliasV1(txJson.as[CreateAliasV1Request], wallet, signerAddress, time)
              case CreateAliasTransactionV2 => TransactionFactory.aliasV2(txJson.as[CreateAliasV2Request], wallet, signerAddress, time)
              case DataTransaction          => TransactionFactory.data(txJson.as[DataRequest], wallet, signerAddress, time)
              case InvokeScriptTransaction =>
                TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], wallet, signerAddress, time)
              case SetScriptTransaction      => TransactionFactory.setScript(txJson.as[SetScriptRequest], wallet, signerAddress, time)
              case SetAssetScriptTransaction => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], wallet, signerAddress, time)
              case SponsorFeeTransaction     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], wallet, signerAddress, time)
              case _                         => Left(ValidationError.UnsupportedTransactionType)
            }
        }).fold(ApiError.fromValidationError, _.json())
    }
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
    ))
  def broadcast: Route = (pathPrefix("broadcast") & post) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        doBroadcast(TransactionFactory.fromSignedRequest(jsv))
      }
    }
  }

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

  def transactionsByAddress(addressParam: String, limitParam: Int, maybeAfterParam: Option[String]): Either[ApiError, JsArray] = {
    def createTransactionsJsonArray(address: Address, limit: Int, fromId: Option[ByteStr]): Either[String, JsArray] = {
      lazy val addressesCached = concurrent.blocking((blockchain.aliasesOfAddress(address) :+ address).toSet)

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

      val txs = concurrent.blocking(blockchain.addressTransactions(address, Set.empty, limit, fromId))
      txs.map(txs => Json.arr(JsArray(txs.map { case (height, tx) => txToCompactJson(address, tx) + ("height" -> JsNumber(height)) })))
    }

    for {
      address <- Address.fromString(addressParam).left.map(ApiError.fromValidationError)
      limit   <- Either.cond(limitParam <= settings.transactionByAddressLimit, limitParam, TooBigArrayAllocation)
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
      result <- createTransactionsJsonArray(address, limit, maybeAfter).fold(
        err => Left(CustomValidationError(err)),
        arr => Right(arr)
      )
    } yield result
  }
}
