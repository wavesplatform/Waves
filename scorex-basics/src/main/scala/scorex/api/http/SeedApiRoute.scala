package scorex.api.http

import java.security.SecureRandom
import javax.ws.rs.Path

import akka.actor.ActorRefFactory
import com.wordnik.swagger.annotations._
import play.api.libs.json.Json
import scorex.app.Application
import scorex.crypto.encode.Base58
import spray.routing.Route

@Api(value = "/seed", description = "Seed generation functions", position = 3)
case class SeedApiRoute(override val application: Application)(implicit val context: ActorRefFactory) extends ApiRoute with CommonApiFunctions {
  val SeedSize = 32

  private def seed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Json.obj("seed" -> Base58.encode(seed)).toString()
  }

  override lazy val route =
    pathPrefix("seed") {
      seedRoute ~ length
    }


  @Path("/")
  @ApiOperation(value = "Seed", notes = "Generate random seed", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json with peer list or error")
  ))
  def seedRoute: Route = path("") {
    jsonRoute {
      seed(SeedSize)
    }
  }

  @Path("/{length}")
  @ApiOperation(value = "Seed of specified length", notes = "Generate random seed of specified length", httpMethod = "GET")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(name = "length", value = "Seed length ", required = true, dataType = "long", paramType = "path")
  ))
  @ApiResponse(code = 200, message = "Json with peer list or error")
  def length: Route = path(IntNumber) { case length =>
    jsonRoute {
      seed(length)
    }
  }


}
