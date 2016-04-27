package scorex.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.{Directives, Route}
import io.swagger.annotations._
import play.api.libs.json.{JsValue, Json}
import scorex.app.Application
import scorex.crypto.encode.Base58

@Path("/seed")
@Api(value = "/seed", description = "Seed generation functions", position = 3, produces = "application/json")
case class SeedApiRoute(override val application: Application)(implicit val context: ActorRefFactory) extends ApiRoute with CommonApiFunctions with Directives {
  val SeedSize = 32

  private def seed(length: Int): JsValue = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed))
  }

  override val route = seedRoute ~ length

  @Path("/")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def seedRoute: Route = path("seed") {
    getJsonRoute {
      seed(SeedSize)
    }
  }

  @Path("/{length}")
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


}
