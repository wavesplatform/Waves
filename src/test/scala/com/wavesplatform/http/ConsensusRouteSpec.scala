package com.wavesplatform.http

import com.wavesplatform.BlockGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import scorex.api.http.BlockNotExists
import scorex.block.Block
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.createTestTemporaryFile
import scorex.crypto.encode.Base58
import scorex.transaction.{BlockStorage, CheckpointService, History, NewTransactionHandler}

class ConsensusRouteSpec extends RouteSpec("/consensus") with RestAPISettingsHelper with PropertyChecks with MockFactory with BlockGen {
  private val bFile = createTestTemporaryFile("waves-blockchain", ".dat")
  private val sFile = createTestTemporaryFile("waves-state", ".dat")
  private val cFile = createTestTemporaryFile("waves-checkpoint", ".dat")
  private val state = mock[StateReader]
  private val history = mock[History]
  (history.height _).expects().returns(10).anyNumberOfTimes()

  private val route = NxtConsensusApiRoute(restAPISettings, state, history, mock[FunctionalitySettings]).route

  routePath("/generationsignature") - {
    "for last block" in {
      forAll(randomSignerBlockGen) { blk =>
        (history.blockAt _).expects(10).returning(Some(blk)).once()
        Get(routePath("/generationsignature")) ~> route ~> check {
          (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual Base58.encode(blk.consensusData.generationSignature)
        }
      }
    }

    "for a given block" in {
      forAll(randomSignerBlockGen, Gen.oneOf(true, false)) { case (blk, isAvailable) =>
        val result = if (isAvailable) Option(blk) else None
        (history.heightOf(_: Block.BlockId)).expects(where(sameSignature(blk.uniqueId)(_))).returning(Some(10)).once()
        (history.blockAt _).expects(*).returning(result).once()
        if (isAvailable) {
          Get(routePath(s"/generationsignature/${blk.encodedId}")) ~> route ~> check {
            (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual Base58.encode(blk.consensusData.generationSignature)
          }
        } else {
          Get(routePath(s"/generationsignature/${blk.encodedId}")) ~> route should produce(BlockNotExists)
        }
      }
    }
  }
  routePath("/basetarget") - {

    "for a given block" in {
      forAll(randomSignerBlockGen, Gen.oneOf(true, false)) { case (blk, isAvailable) =>
        val result = if (isAvailable) Option(blk) else None
        (history.heightOf(_: Block.BlockId)).expects(where(sameSignature(blk.uniqueId)(_))).returning(Some(10)).once()
        (history.blockAt _).expects(*).returning(result).once()
        if (isAvailable) {
          Get(routePath(s"/basetarget/${blk.encodedId}")) ~> route ~> check {
            (responseAs[JsObject] \ "baseTarget").as[Long] shouldEqual blk.consensusDataField.value.baseTarget
          }
        } else {
          Get(routePath(s"/basetarget/${blk.encodedId}")) ~> route should produce(BlockNotExists)
        }
      }
    }
  }
}
