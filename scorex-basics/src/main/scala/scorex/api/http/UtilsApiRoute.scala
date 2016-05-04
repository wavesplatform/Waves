package scorex.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import io.swagger.annotations._
import play.api.libs.json.{JsValue, Json}
import scorex.app.Application
import scorex.crypto.encode.Base58
import scorex.crypto.hash.{FastCryptographicHash, SecureCryptographicHash}

@Path("/utils")
@Api(value = "/utils", description = "Useful functions", position = 3, produces = "application/json")
case class UtilsApiRoute(override val application: Application)(implicit val context: ActorRefFactory) extends ApiRoute {
  val SeedSize = 32

  private def seed(length: Int): JsValue = {
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
  def seedRoute: Route = path("seed") {
    getJsonRoute {
      seed(SeedSize)
    }
  }

  @Path("/seed/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "long", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Json with peer list or error")
  def length: Route = path("seed" / IntNumber) { case length =>
    getJsonRoute {
      seed(length)
    }
  }

  @Path("/hash/secure")
  @ApiOperation(value = "Hash", notes = "Return FastCryptographicHash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "String")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashFast: Route = {
    path("hash" / "secure") {
      entity(as[String]) { message =>
        withAuth {
          postJsonRoute {
            Json.obj("message" -> message, "hash" -> Base58.encode(SecureCryptographicHash(message)))
          }
        }
      }
    }
  }

  @Path("/hash/fast")
  @ApiOperation(value = "Hash", notes = "Return  SecureCryptographicHash of specified message", httpMethod = "POST")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "message", value = "Message to hash", required = true, paramType = "body", dataType = "String")
  ))
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with error or json like {\"message\": \"your message\",\"hash\": \"your message hash\"}")
  ))
  def hashSecure: Route = {
    path("hash" / "fast") {
      entity(as[String]) { message =>
        withAuth {
          postJsonRoute {
            Json.obj("message" -> message, "hash" -> Base58.encode(FastCryptographicHash(message)))
          }
        }
      }
    }
  }

}
