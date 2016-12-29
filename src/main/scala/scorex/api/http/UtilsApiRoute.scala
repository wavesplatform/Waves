package scorex.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.Json
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{FastCryptographicHash, SecureCryptographicHash}

@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(application: Application)(implicit val context: ActorRefFactory)
  extends ApiRoute {
  val settings = application.settings
  val MaxSeedSize = 1024
  val DefaultSeedSize = 32

  private def seed(length: Int): JsonResponse = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    val json = Json.obj("seed" -> Base58.encode(seed))
    JsonResponse(json, StatusCodes.OK)
  }

  override val route = pathPrefix("utils") {
    seedRoute ~ length ~ hashFast ~ hashSecure
  }

  @Path("/seed")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def seedRoute: Route = path("seed") {
    getJsonRoute {
      seed(DefaultSeedSize)
    }
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "integer", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Json with error message")
  def length: Route = path("seed" / IntNumber) { case length =>
    getJsonRoute {
      if (length <= MaxSeedSize) seed(length)
      else TooBigArrayAllocation.response
    }
  }

  @Path("/hash/secure")
  @ApiOperation(value = "Hash", notes = "Return SecureCryptographicHash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "string")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashFast: Route = {
    path("hash" / "secure") {
      entity(as[String]) { message =>
        postJsonRoute {
          val json = Json.obj("message" -> message, "hash" -> Base58.encode(SecureCryptographicHash(message)))
          JsonResponse(json, StatusCodes.OK)
        }
      }
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
  def hashSecure: Route = {
    path("hash" / "fast") {
      entity(as[String]) { message =>
        postJsonRoute {
          val json = Json.obj("message" -> message, "hash" -> Base58.encode(FastCryptographicHash(message)))
          JsonResponse(json, StatusCodes.OK)
        }
      }
    }
  }

}
