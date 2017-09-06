package scorex.api.http.alias

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.UtxPool
import com.wavesplatform.settings.RestAPISettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import play.api.libs.json.{Format, Json}
import scorex.BroadcastRoute
import scorex.account.Alias
import scorex.api.http._
import scorex.transaction._
import scorex.utils.Time
import scorex.wallet.Wallet

@Path("/alias")
@Api(value = "/alias")
case class AliasApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, state: SnapshotStateReader)
  extends ApiRoute with BroadcastRoute {

  override val route = pathPrefix("alias") {
    alias ~ addressOfAlias ~ aliasOfAddress
  }

  @Path("/create")
  @ApiOperation(value = "Creates an alias",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.alias.CreateAliasRequest",
      defaultValue = "{\n\t\"alias\": \"aliasalias\",\n\t\"sender\": \"3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7\",\n\t\"fee\": 100000\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def alias: Route = processRequest("create", (t: CreateAliasRequest) => doBroadcast(TransactionFactory.alias(t, wallet, time)))


  @Path("/by-alias/{alias}")
  @ApiOperation(value = "Account", notes = "Returns an address associated with an Alias. Alias should be plain text without an 'alias' prefix and network code.", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "alias", value = "Alias", required = true, dataType = "string", paramType = "path")
  ))
  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    val result = Alias.buildWithCurrentNetworkByte(aliasName) match {
      case Right(alias) =>
        state.resolveAlias(alias) match {
          case Some(addr) => Right(Address(addr.stringRepr))
          case None => Left(AliasDoesNotExist(alias))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/by-address/{address}")
  @ApiOperation(value = "Alias", notes = "Returns a collection of aliases associated with an Address", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "address", value = "3Mx2afTZ2KbRrLNbytyzTtXukZvqEB8SkW7", required = true, dataType = "string", paramType = "path")
  ))
  def aliasOfAddress: Route = (get & path("by-address" / Segment)) { addressString =>
    val result: Either[ApiError, Seq[String]] = scorex.account.Address.fromString(addressString)
      .map(acc => state.aliasesOfAddress(acc).map(_.stringRepr))
      .left.map(ApiError.fromValidationError)
    complete(result)
  }

  case class Address(address: String)

  implicit val addressFormat: Format[Address] = Json.format
}
