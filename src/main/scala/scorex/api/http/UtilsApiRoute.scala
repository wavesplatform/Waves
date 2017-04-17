package scorex.api.http

import java.security.SecureRandom
import javax.ws.rs.Path
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, PredefinedFromEntityUnmarshallers}
import com.wavesplatform.settings.RestAPISettings
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{FastCryptographicHash, SecureCryptographicHash}

@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(settings: RestAPISettings) extends ApiRoute {
  val MaxSeedSize = 1024
  val DefaultSeedSize = 32

  private def seed(length: Int) = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashFast ~ hashSecure
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def seedRoute: Route = (path("seed") & get) {
    complete(seed(DefaultSeedSize))
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "integer", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Json with error message")
  def length: Route = (path("seed" / IntNumber) & get) { length =>
      if (length <= MaxSeedSize) complete(seed(length))
      else complete(TooBigArrayAllocation)
  }

  @Path("/hash/secure")
  @ApiOperation(value = "Hash", notes = "Return SecureCryptographicHash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashFast: Route = (path("hash" / "secure") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(SecureCryptographicHash(message))))
    }
  }

  @Path("/hash/fast")
  @ApiOperation(value = "Hash", notes = "Return FastCryptographicHash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashSecure: Route = (path("hash" / "fast") & post) {
    entity(as[String]) { message =>
      complete(Json.obj("message" -> message, "hash" -> Base58.encode(FastCryptographicHash(message))))
    }
  }
}
