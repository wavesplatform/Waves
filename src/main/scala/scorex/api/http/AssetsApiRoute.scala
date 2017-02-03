package scorex.api.http

import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json._
import scorex.account.Account
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Order
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.wallet._
import scorex.transaction.{AssetAcc, SimpleTransactionModule, StateCheckFailed, ValidationError}
import scorex.transaction.assets.exchange.OrderJson._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

@Path("/assets")
@Api(value = "assets")
case class AssetsApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute with CommonTransactionApiFunctions {
  val MaxAddressesPerRequest = 1000

  val settings = application.settings

  private val wallet = application.wallet
  private val state = application.blockStorage.state.asInstanceOf[StoredState]
  private implicit val transactionModule = application.transactionModule.asInstanceOf[SimpleTransactionModule]

  override lazy val route =
    pathPrefix("assets") {
      balance ~ balances ~ issue ~ reissue ~ burnRoute ~ transfer ~ signOrder
    }

  @Path("/balance/{address}/{assetId}")
  @ApiOperation(value = "Asset's balance", notes = "Account's balance by given asset", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path"),
    new ApiImplicitParam(name = "assetId", value = "Asset ID", required = true, dataType = "string", paramType = "path")
  ))
  def balance: Route = {
    path("balance" / Segment / Segment) { case (address, assetId) =>
      getJsonRoute {
        balanceJson(address, assetId)
      }
    }
  }

  @Path("/balance/{address}")
  @ApiOperation(value = "Account's balance", notes = "Account's balances for all assets", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
  ))
  def balances: Route = {
    path("balance" / Segment) { address =>
      getJsonRoute {
        balanceJson(address)
      }
    }
  }

  @Path("/transfer")
  @ApiOperation(value = "Transfer asset",
    notes = "Transfer asset to new address",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.TransferRequest",
      defaultValue = "{\"sender\":\"3Mn6xomsZZepJj1GL1QaW6CaCJAq8B3oPef\",\"recipient\":\"3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk\",\"assetId\":null,\"amount\":5813874260609385500,\"feeAssetId\":\"3Z7T9SwMbcBuZgcn3mGu7MMp619CTgSWBT7wvEkPwYXGnoYzLeTyh3EqZu1ibUhbUHAsGK5tdv9vJL9pk4fzv9Gc\",\"fee\":1579331567487095949,\"timestamp\":4231642878298810008}"
    )
  ))
  def transfer: Route = path("transfer") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[TransferRequest] match {
              case err: JsError =>
                WrongTransactionJson(err).response
              case JsSuccess(request: TransferRequest, _) =>
                val txOpt = transactionModule.transferAsset(request, wallet)
                txOpt match {
                  case Success(txVal) =>
                    txVal match {
                      case Right(tx) => JsonResponse(tx.json, StatusCodes.OK)
                      case Left(e) => WrongJson.response
                    }
                  case Failure(e: StateCheckFailed) =>
                    StateCheckFailed.response
                  case _ =>
                    WrongJson.response
                }
            }
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }

  @Path("/issue")
  @ApiOperation(value = "Issue Asset",
    notes = "Issue new Asset",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.IssueRequest",
      defaultValue = "{\"sender\":\"string\",\"name\":\"str\",\"description\":\"string\",\"quantity\":100000,\"decimals\":7,\"reissuable\":false,\"fee\":100000000}"
    )
  ))
  def issue: Route = path("issue") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[IssueRequest] match {
              case err: JsError =>
                WrongTransactionJson(err).response
              case JsSuccess(issue: IssueRequest, _) =>
                val txOpt: Try[IssueTransaction] = transactionModule.issueAsset(issue, wallet)
                txOpt match {
                  case Success(tx) =>
                    JsonResponse(tx.json, StatusCodes.OK)
                  case Failure(e: StateCheckFailed) =>
                    StateCheckFailed.response
                  case _ =>
                    WrongJson.response
                }
            }
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }

  @Path("/reissue")
  @ApiOperation(value = "Issue Asset",
    notes = "Reissue Asset",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.ReissueRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100000,\"reissuable\":false,\"fee\":1}"
    )
  ))
  def reissue: Route = path("reissue") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[ReissueRequest] match {
              case err: JsError =>
                WrongTransactionJson(err).response
              case JsSuccess(issue: ReissueRequest, _) =>
                val txOpt: Try[ReissueTransaction] = transactionModule.reissueAsset(issue, wallet)
                txOpt match {
                  case Success(tx) =>
                    JsonResponse(tx.json, StatusCodes.OK)
                  case Failure(e: StateCheckFailed) =>
                    StateCheckFailed.response
                  case _ =>
                    WrongJson.response
                }
            }
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }

  @Path("/burn")
  @ApiOperation(value = "Burn Asset",
    notes = "Burn some of your assets",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.state.wallet.BurnRequest",
      defaultValue = "{\"sender\":\"string\",\"assetId\":\"Base58\",\"quantity\":100,\"fee\":100000}"
    )
  ))
  def burnRoute: Route = path("burn") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[BurnRequest] match {
              case err: JsError =>
                WrongTransactionJson(err).response
              case JsSuccess(burnRequest: BurnRequest, _) =>
                val txOpt: Try[BurnTransaction] = transactionModule.burnAsset(burnRequest, wallet)
                txOpt match {
                  case Success(tx) =>
                    JsonResponse(tx.json, StatusCodes.OK)
                  case Failure(e: StateCheckFailed) =>
                    StateCheckFailed.response
                  case _ =>
                    WrongJson.response
                }
            }
          }.getOrElse(WrongJson.response)
        }
      }
    }
  }


  private def balanceJson(address: String, assetIdStr: String): JsonResponse = {
    val account = new Account(address)
    Base58.decode(assetIdStr) match {
      case Success(assetId) if Account.isValid(account) =>
        val json = Json.obj(
          "address" -> account.address,
          "assetId" -> assetIdStr,
          "balance" -> state.assetBalance(AssetAcc(account, Some(assetId)))
        )
        JsonResponse(json, StatusCodes.OK)
      case _ => InvalidAddress.response
    }
  }

  private def balanceJson(address: String): JsonResponse = {
    val account = new Account(address)
    if (Account.isValid(account)) {
      val balances: Seq[JsObject] = state.getAccountBalance(account).map { p =>
        JsObject(Seq(
          "assetId" -> JsString(Base58.encode(p._1)),
          "balance" -> JsNumber(p._2._1),
          "reissuable" -> JsBoolean(p._2._2),
          "quantity" -> JsNumber(p._2._3),
          "issueTransaction" -> p._2._4.json
        ))
      }.toSeq
      val json = Json.obj(
        "address" -> account.address,
        "balances" -> JsArray(balances)
      )
      JsonResponse(json, StatusCodes.OK)
    } else InvalidAddress.response
  }

  @Path("/order")
  @ApiOperation(value = "Sign Order",
    notes = "Create order signed by address from wallet",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Order Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.transaction.assets.exchange.Order"
    )
  ))
  def signOrder: Route = path("order") {
    entity(as[String]) { body =>
      withAuth {
        postJsonRoute {
          Try(Json.parse(body)).map { js =>
            js.validate[Order] match {
              case err: JsError =>
                Future.successful(WrongTransactionJson(err).response)
              case JsSuccess(order: Order, _) =>
                Future {
                  wallet.privateKeyAccount(order.sender.address).map { sender =>
                    val signed = Order.sign(order, sender)
                    JsonResponse(signed.json, StatusCodes.OK)
                  }.getOrElse(InvalidAddress.response)
                }
            }
          }.recover {
            case t =>
              Future.successful(WrongJson.response)
          }.get
        }
      }
    }
  }

}
