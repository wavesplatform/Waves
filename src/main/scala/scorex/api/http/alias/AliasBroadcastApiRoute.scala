package scorex.api.http.alias

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import scorex.BroadcastRoute
import scorex.api.http._
import scorex.transaction._

@Path("/alias/broadcast")
@Api(value = "/alias")
case class AliasBroadcastApiRoute(settings: RestAPISettings, transactionModule: TransactionModule)
  extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  @Path("/create")
  @ApiOperation(value = "Broadcasts a signed alias transaction",
    httpMethod = "POST",
    produces = "application/json",
    consumes = "application/json")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Json with data",
      required = true,
      paramType = "body",
      dataType = "scorex.api.http.alias.SignedAliasRequest",
      defaultValue = "{\n\t\"alias\": \"aliasalias\",\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000\n\t\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
    )
  ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedCreate: Route =  json[SignedCreateAliasRequest] { aliasReq =>
    doBroadcast(aliasReq.toTx)
  }
}
