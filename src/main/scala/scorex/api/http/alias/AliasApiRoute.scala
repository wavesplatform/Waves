package scorex.api.http.alias

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, Alias}
import scorex.api.http._
import scorex.transaction._
import scorex.wallet.Wallet

@Path("/alias")
@Api(value = "/alias")
case class AliasApiRoute(settings: RestAPISettings, wallet: Wallet, transactionOperations: TransactionOperations, state: State)
  extends ApiRoute {

  override val route = pathPrefix("alias") {
    alias ~ addressOfAlias ~ aliasOfAddress
  }

  @Path("/create")
  @ApiOperation(value = "Creates a alias",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.alias.AliasRequest",
      defaultValue = "{\n\t\"alias\": \"aliasalias\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def alias: Route = processRequest("create", (t: AliasRequest) => transactionOperations.alias(t, wallet))


  @Path("/byAlias/{alias}")
  @ApiOperation(value = "Account", notes = "Address by Alias", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "alias", value = "Alias", required = true, dataType = "string", paramType = "path")
  ))
  def addressOfAlias: Route = (get & path("byAlias" / Segment)) { aliasString =>
    val result = Alias(aliasString) match {
      case Right(alias) =>
        state.resolveAlias(alias) match {
          case Some(addr) => Right(AliasInfo(addr.address, aliasString))
          case None => Left(AliasNotExists(alias))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/byAddress/{address}")
  @ApiOperation(value = "Alias", notes = "Alias by Address", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7", required = true, dataType = "string", paramType = "path")
  ))
  def aliasOfAddress: Route = (get & path("byAddress" / Segment)) { addressString =>
    val result: Either[ApiError, AliasInfo] = Account.fromBase58String(addressString) match {
      case Right(address) =>
        state.getAlias(address) match {
          case Some(al) => Right(AliasInfo(address.stringRepr, al.stringRepr))
          case None => Left(AliasNotExists(address))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  case class AliasInfo(address: String, alias: String)

  implicit val aliasInfoFormat: Format[AliasInfo] = Json.format
}
