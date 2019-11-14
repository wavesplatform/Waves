package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.BlockGen
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.BlockDoesNotExist
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.consensus.nxt.api.http.NxtConsensusApiRoute
import com.wavesplatform.db.WithDomain
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.state._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}
import play.api.libs.json.JsObject

class ConsensusRouteSpec
    extends RouteSpec("/consensus")
    with RestAPISettingsHelper
    with PropertyChecks
    with BlockGen
    with HistoryTest
    with WithDomain {

  private def routeTest(f: (Blockchain, Route) => Any) = withDomain() { d =>
    d.blockchainUpdater.processBlock(genesisBlock, genesisBlock.header.generationSignature)
    1 to 10 foreach { _ =>
      val block = getNextTestBlock(d.blockchainUpdater)
      d.blockchainUpdater.processBlock(block, block.header.generationSignature)
    }
    val commonApi = CommonBlocksApi(d.blockchainUpdater, _ => None)
    f(d.blockchainUpdater, NxtConsensusApiRoute(restAPISettings, d.blockchainUpdater, commonApi).route)
  }

  routePath("/generationsignature") - {
    "for last block" in routeTest { (h, route) =>
      Get(routePath("/generationsignature")) ~> route ~> check {
        (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual h.lastBlockHeader.get.header.generationSignature.toString
      }
    }

    "for existing block" in routeTest { (h, route) =>
      val SignedBlockHeader(header, id) = h.blockHeader(3).get
      Get(routePath(s"/generationsignature/$id")) ~> route ~> check {
        (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual header.generationSignature.toString
      }
    }

    "for non-existent block" in routeTest { (h, route) =>
      Get(routePath(s"/generationsignature/brggwg4wg4g")) ~> route should produce(BlockDoesNotExist)
    }
  }

  routePath("/basetarget") - {
    "for existing block" in routeTest { (h, route) =>
      val SignedBlockHeader(header, id) = h.blockHeader(3).get
      Get(routePath(s"/basetarget/$id")) ~> route ~> check {
        (responseAs[JsObject] \ "baseTarget").as[Long] shouldEqual header.baseTarget
      }
    }

    "for non-existent block" in routeTest { (h, route) =>
      Get(routePath(s"/basetarget/brggwg4wg4g")) ~> route should produce(BlockDoesNotExist)
    }
  }
}
