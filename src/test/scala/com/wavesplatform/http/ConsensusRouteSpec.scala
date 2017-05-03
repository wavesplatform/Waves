package com.wavesplatform.http

import com.wavesplatform.BlockGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, GenesisSettings}
import com.wavesplatform.state2.reader.StateReader
import org.scalacheck.Gen
import org.scalamock.scalatest.MockFactory
import org.scalatest.DoNotDiscover
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import scorex.api.http.BlockNotExists
import scorex.block.Block
import scorex.consensus.nxt.WavesConsensusModule
import scorex.consensus.nxt.api.http.NxtConsensusApiRoute
import scorex.createTestTemporaryFile
import scorex.crypto.encode.Base58
import scorex.transaction.{BlockStorage, CheckpointService, History, TransactionModule}

class ConsensusRouteSpec extends RouteSpec("/consensus") with RestAPISettingsHelper with PropertyChecks with MockFactory with BlockGen {
  private val bFile = createTestTemporaryFile("waves-blockchain", ".dat")
  private val sFile = createTestTemporaryFile("waves-state", ".dat")
  private val cFile = createTestTemporaryFile("waves-checkpoint", ".dat")
  private val wcm = new WavesConsensusModule(BlockchainSettings(bFile.getAbsolutePath, sFile.getAbsolutePath, cFile.getAbsolutePath, 'T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET))
  private val state = mock[StateReader]
  private val history = stub[History]
  (history.height _).when().returns(10)

  private val bs: BlockStorage = new BlockStorage {
    override def history = ConsensusRouteSpec.this.history

    override def blockchainUpdater = ???

    override def stateReader = state

    override def checkpoints: CheckpointService = ???
  }
  private val tm = mock[TransactionModule]
  (tm.blockStorage _).expects().returning(bs)

  private val route = NxtConsensusApiRoute(restAPISettings, wcm, state, history, tm).route

  routePath("/generationsignature") - {
    "for last block" in {
      forAll(randomSignerBlockGen) { blk =>
        (history.height _).when().returns(10)
        (history.blockAt _).when(10).returns(Some(blk))
        Get(routePath("/generationsignature")) ~> route ~> check {
          (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual Base58.encode(blk.consensusData.generationSignature)
        }
      }
    }

    "for a given block" in {
      forAll(randomSignerBlockGen, Gen.oneOf(true, false)) { case (blk, isAvailable) =>
        val result = if (isAvailable) Option(blk) else None
        (history.heightOf(_: Block.BlockId)).when(where(sameSignature(blk.uniqueId)(_))).returns(Some(10)).once()
        (history.blockAt _).when(*).returns(result).once()
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
        (history.heightOf(_: Block.BlockId)).when(where(sameSignature(blk.uniqueId)(_))).returns(Some(10)).once()
        (history.blockAt _).when(*).returns(result).once()
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
