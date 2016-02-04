package scorex.lagonaki

import dispatch.{Http, url}
import org.scalatest.{BeforeAndAfterAll, Suites}
import play.api.libs.json.{JsObject, Json}
import scorex.lagonaki.TestingCommons._
import scorex.lagonaki.integration.{APISpecification, BlockGeneratorSpecification, BlocksRoutingSpecification, ValidChainGenerationSpecification}
import scorex.lagonaki.props.BlockStorageSpecification
import scorex.lagonaki.unit._
import scorex.transaction.state.database.blockchain.BlockTreeSpecification
import scorex.utils._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class LagonakiTestSuite extends Suites(
  //unit tests
  new MessageSpecification
  , new BlockSpecification
  , new BlockStorageSpecification
  , new WalletSpecification
  , new BlockGeneratorSpecification
  , new BlocksRoutingSpecification
  , new BlockTreeSpecification

  //integration tests - slow!
  , new ValidChainGenerationSpecification
  , new APISpecification

) with BeforeAndAfterAll {

  override protected def beforeAll() = {
    applications.foreach { a =>
      a.run()
      if (a.wallet.privateKeyAccounts().isEmpty) a.wallet.generateNewAccounts(3)
      untilTimeout(20.seconds, 1.second) {
        val request = Http(url(peerUrl(a) + "/scorex/status").GET)
        val response = Await.result(request, 10.seconds)
        val json = Json.parse(response.getResponseBody).as[JsObject]
        assert((json \ "block generator status").asOpt[String].isDefined)
      }
    }
  }

  override protected def afterAll() = {
    applications.foreach(_.stopAll())
  }
}
