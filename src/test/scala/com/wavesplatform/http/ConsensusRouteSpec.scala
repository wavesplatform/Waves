package com.wavesplatform.http

import com.wavesplatform.BlockGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import scorex.api.http.BlockNotExists
import scorex.block.Block
import scorex.consensus.nxt.WavesConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.createTestTemporaryFile
import scorex.crypto.encode.Base58
import scorex.transaction.{History, State, TransactionModule}

class ConsensusRouteSpec extends RouteSpec("/consensus") with RestAPISettingsHelper with PropertyChecks with MockFactory with BlockGen {
  val bcFile = createTestTemporaryFile("blockchain", ".dat")
  val wcm = new WavesConsensusModule(BlockchainSettings(bcFile.getAbsolutePath, 'T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET))
  val state = mock[State]
  val history = mock[History]
  val tm = mock[TransactionModule]
  val route = NxtConsensusApiRoute(restAPISettings, wcm, state, history, tm).route

  routePath("/generationsignature") - {
    "for last block" in {
      forAll(blockGen) { blk =>
        (history.lastBlock _).expects().returning(blk).once()
        Get(routePath("/generationsignature")) ~> route ~> check {
          (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual Base58.encode(blk.consensusData.generationSignature)
        }
      }
    }

    "for a given block" in {
      forAll(blockGen, Gen.oneOf(true, false)) { case (blk, isAvailable) =>
        val result = if (isAvailable) Option(blk) else None
        (history.blockById(_: Block.BlockId)).expects(where(sameSignature(blk.uniqueId)(_))).returning(result).once()
        if (isAvailable) {
          Get(routePath(s"/generationsignature/${blk.encodedId}")) ~> route ~> check {
            (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual Base58.encode(blk.consensusData.generationSignature)
          }
        } else {
          Get(routePath(s"/generationsignature/${blk.encodedId}")) ~> route should produce (BlockNotExists)
        }
      }
    }
  }
  routePath("/basetarget") - {
    "for last block" in {
      forAll(blockGen) { blk =>
        (history.lastBlock _).expects().returning(blk).once()
        Get(routePath("/basetarget")) ~> route ~> check {
          (responseAs[JsObject] \ "baseTarget").as[Long] shouldEqual blk.consensusDataField.value.baseTarget
        }
      }
    }

    "for a given block" in {
      forAll(blockGen, Gen.oneOf(true, false)) { case (blk, isAvailable) =>
        val result = if (isAvailable) Option(blk) else None
        (history.blockById(_: Block.BlockId)).expects(where(sameSignature(blk.uniqueId)(_))).returning(result).once()
        if (isAvailable) {
          Get(routePath(s"/basetarget/${blk.encodedId}")) ~> route ~> check {
            (responseAs[JsObject] \ "baseTarget").as[Long] shouldEqual blk.consensusDataField.value.baseTarget
          }
        } else {
          Get(routePath(s"/basetarget/${blk.encodedId}")) ~> route should produce (BlockNotExists)
        }
      }
    }
  }

  routePath("/generatingbalance/{address}") - {
    forAll(accountGen) { account =>
      (state.effectiveBalanceWithConfirmations _).expects(*,*,*).returning(0).once()
      Get(routePath(s"/generatingbalance/${account.address}")) ~> route ~> check {

      }
    }
  }
}
