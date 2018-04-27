package scorex.api.http

import java.util.NoSuchElementException

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{ExceptionHandler, Route}
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state.{Blockchain, ByteStr}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json._
import scorex.BroadcastRoute
import scorex.account.Address
import scorex.api.http.DataRequest._
import scorex.api.http.alias.{CreateAliasRequest, SignedCreateAliasRequest}
import scorex.api.http.assets._
import scorex.api.http.assets.SponsorFeeRequest._
import scorex.api.http.leasing.{LeaseCancelRequest, LeaseRequest, SignedLeaseCancelRequest, SignedLeaseRequest}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.transaction.assets._
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.transfer._
import scorex.utils.Time
import scorex.wallet.Wallet

import scala.util.Success
import scala.util.control.Exception

@Path("/transactions")
@Api(value = "/transactions")
case class TransactionsApiRoute(settings: RestAPISettings,
                                wallet: Wallet,
                                blockchain: Blockchain,
                                utx: UtxPool,
                                allChannels: ChannelGroup,
                                time: Time)
    extends ApiRoute
    with BroadcastRoute
    with CommonApiFunctions {

  import TransactionsApiRoute.MaxTransactionsPerRequest

  override lazy val route =
    pathPrefix("transactions") {
      unconfirmed ~ addressLimit ~ info ~ sign ~ broadcast
    }

  private val invalidLimit = StatusCodes.BadRequest -> Json.obj("message" -> "invalid.limit")

  //TODO implement general pagination
  @Path("/address/{address}/limit/{limit}")
  @ApiOperation(value = "Address", notes = "Get list of transactions where specified address has been involved", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Wallet address ", required = true, dataType = "string", paramType = "path"),
      new ApiImplicitParam(name = "limit",
                           value = "Specified number of records to be returned",
                           required = true,
                           dataType = "integer",
                           paramType = "path")
    ))
  def addressLimit: Route = (pathPrefix("address") & get) {
    pathPrefix(Segment) { address =>
      Address.fromString(address) match {
        case Left(e) => complete(ApiError.fromValidationError(e))
        case Right(a) =>
          pathPrefix("limit") {
            pathEndOrSingleSlash {
              complete(invalidLimit)
            } ~
              path(Segment) { limitStr =>
                Exception.allCatch.opt(limitStr.toInt) match {
                  case Some(limit) if limit > 0 && limit <= MaxTransactionsPerRequest =>
                    complete(Json.arr(JsArray(blockchain.addressTransactions(a, Set.empty, limit, 0).map {
                      case (h, tx) =>
                        txToCompactJson(a, tx) + ("height" -> JsNumber(h))
                    })))
                  case Some(limit) if limit > MaxTransactionsPerRequest =>
                    complete(TooBigArrayAllocation)
                  case _ =>
                    complete(invalidLimit)
                }
              }
          } ~ complete(StatusCodes.NotFound)
      }
    }
  }

  @Path("/info/{id}")
  @ApiOperation(value = "Info", notes = "Get transaction info", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "transaction id ", required = true, dataType = "string", paramType = "path")
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
  @ApiOperation(value = "Unconfirmed", notes = "Get list of unconfirmed transactions", httpMethod = "GET")
  def unconfirmed: Route = (pathPrefix("unconfirmed") & get) {
    pathEndOrSingleSlash {
      complete(JsArray(utx.all.map(txToExtendedJson)))
    } ~ utxSize ~ utxTransactionInfo
  }

  @Path("/unconfirmed/size")
  @ApiOperation(value = "Size of UTX pool", notes = "Get number of unconfirmed transactions in the UTX pool", httpMethod = "GET")
  def utxSize: Route = (pathPrefix("size") & get) {
    complete(Json.obj("size" -> JsNumber(utx.size)))
  }

  @Path("/unconfirmed/info/{id}")
  @ApiOperation(value = "Transaction Info", notes = "Get transaction that is in the UTX", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "id", value = "Transaction id ", required = true, dataType = "string", paramType = "path")
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

  @Path("/sign")
  @ApiOperation(value = "Sign a transaction", notes = "Sign a transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           required = true,
                           dataType = "string",
                           paramType = "body",
                           value = "Transaction data including type and optional timestamp in milliseconds")
    ))
  def sign: Route = (pathPrefix("sign") & post & withAuth) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        val typeId  = (jsv \ "type").as[Byte]
        val version = (jsv \ "version").asOpt[Byte].getOrElse(1.toByte)

        val r = TransactionParsers.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            x match {
              case IssueTransactionV1      => TransactionFactory.issueAsset(jsv.as[IssueRequest], wallet, time)
              case TransferTransactionV1   => TransactionFactory.transferAsset(jsv.as[TransferRequest], wallet, time)
              case TransferTransactionV2   => TransactionFactory.versionedTransfer(jsv.as[VersionedTransferRequest], wallet, time)
              case MassTransferTransaction => TransactionFactory.massTransferAsset(jsv.as[MassTransferRequest], wallet, time)
              case ReissueTransaction      => TransactionFactory.reissueAsset(jsv.as[ReissueRequest], wallet, time)
              case BurnTransaction         => TransactionFactory.burnAsset(jsv.as[BurnRequest], wallet, time)
              case LeaseTransaction        => TransactionFactory.lease(jsv.as[LeaseRequest], wallet, time)
              case LeaseCancelTransaction  => TransactionFactory.leaseCancel(jsv.as[LeaseCancelRequest], wallet, time)
              case CreateAliasTransaction  => TransactionFactory.alias(jsv.as[CreateAliasRequest], wallet, time)
              case DataTransaction         => TransactionFactory.data(jsv.as[DataRequest], wallet, time)
              case IssueTransactionV2      => TransactionFactory.smartIssue(jsv.as[SmartIssueRequest], wallet, time)
              case SetScriptTransaction    => TransactionFactory.setScript(jsv.as[SetScriptRequest], wallet, time)
              case SponsorFeeTransaction   => TransactionFactory.sponsor(jsv.as[SponsorFeeRequest], wallet, time)
            }
        }
        r match {
          case Right(tx) => tx.json()
          case Left(err) => ApiError.fromValidationError(err)
        }
      }
    }
  }

  @Path("/broadcast")
  @ApiOperation(value = "Broadcasts a signed transaction", notes = "Broadcasts a signed transaction", httpMethod = "POST")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "json",
                           value = "Transaction data including type and signature",
                           required = true,
                           dataType = "string",
                           paramType = "body")
    ))
  def broadcast: Route = (pathPrefix("broadcast") & post) {
    handleExceptions(jsonExceptionHandler) {
      json[JsObject] { jsv =>
        val typeId  = (jsv \ "type").as[Byte]
        val version = (jsv \ "version").asOpt[Byte].getOrElse(1.toByte)

        val r = TransactionParsers.by(typeId, version) match {
          case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
          case Some(x) =>
            x match {
              case IssueTransactionV1      => jsv.as[SignedIssueRequest].toTx
              case TransferTransactionV1   => jsv.as[SignedTransferRequest].toTx
              case TransferTransactionV2   => jsv.as[SignedVersionedTransferRequest].toTx
              case MassTransferTransaction => jsv.as[SignedMassTransferRequest].toTx
              case ReissueTransaction      => jsv.as[SignedReissueRequest].toTx
              case BurnTransaction         => jsv.as[SignedBurnRequest].toTx
              case LeaseTransaction        => jsv.as[SignedLeaseRequest].toTx
              case LeaseCancelTransaction  => jsv.as[SignedLeaseCancelRequest].toTx
              case CreateAliasTransaction  => jsv.as[SignedCreateAliasRequest].toTx
              case DataTransaction         => jsv.as[SignedDataRequest].toTx
              case IssueTransactionV2      => jsv.as[SignedSmartIssueRequest].toTx
              case SetScriptTransaction    => jsv.as[SignedSetScriptRequest].toTx
              case SponsorFeeTransaction   => jsv.as[SignedSponsorFeeRequest].toTx

            }
        }
        doBroadcast(r)
      }
    }
  }

  private def txToExtendedJson(tx: Transaction): JsObject = {
    import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
    tx match {
      case lease: LeaseTransaction =>
        import LeaseTransaction.Status._
        lease.json() ++ Json.obj("status" -> (if (blockchain.leaseDetails(lease.id()).exists(_.isActive)) Active else Canceled))
      case leaseCancel: LeaseCancelTransaction =>
        leaseCancel.json() ++ Json.obj("lease" -> blockchain.transactionInfo(leaseCancel.leaseId).map(_._2.json()).getOrElse[JsValue](JsNull))
      case t => t.json()
    }
  }

  /**
    * Produces compact representation for large transactions by stripping unnecessary data.
    * Currently implemented for MassTransfer transaction only.
    */
  private def txToCompactJson(address: Address, tx: Transaction): JsObject = {
    import scorex.transaction.transfer._
    tx match {
      case mtt: MassTransferTransaction if mtt.sender.toAddress != address => mtt.compactJson(address)
      case _                                                               => txToExtendedJson(tx)
    }
  }

  private val jsonExceptionHandler = ExceptionHandler {
    case JsResultException(err)    => complete(WrongJson(errors = err))
    case e: NoSuchElementException => complete(WrongJson(Some(e)))
  }
}

object TransactionsApiRoute {
  val MaxTransactionsPerRequest = 10000
}
