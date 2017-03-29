package com.wavesplatform.http

import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{CheckpointsSettings, RestAPISettings}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.api.http.{BlocksApiRoute, TooBigArrayAllocation}
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.crypto.encode.Base58
import scorex.network.Coordinator.BroadcastCheckpoint
import scorex.transaction.{History, TransactionGen}

class BlocksRouteSpec extends RouteSpec("/blocks") with MockFactory with TransactionGen with PropertyChecks {
  private val config = ConfigFactory.load()
  private val restSettings = RestAPISettings.fromConfig(config)
  private val checkpointSettings = CheckpointsSettings.fromConfig(config)
  private val probe = TestProbe()
  private val history = mock[History]
  private val route = BlocksApiRoute(restSettings, checkpointSettings, history, probe.ref).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private def byteArrayGen(length: Int): Gen[Array[Byte]] = Gen.listOfN(length, Arbitrary.arbitrary[Byte]).map(_.toArray)

  private val blockGen = for {
    signer <- accountGen
    tr <- transferGenerator
    iss <- issueGenerator
    ri <- reissueGenerator
    ca <- createAliasGenerator
    txCount <- Gen.choose(10, 50)
    txs <- Gen.listOfN(txCount, Gen.oneOf(tr, iss, ri, ca))
    ts = txs.map(_.timestamp).max
    reference <- byteArrayGen(Block.BlockIdLength)
    baseTarget <- Gen.posNum[Long]
    generationSignature <- byteArrayGen(WavesConsensusModule.GeneratorSignatureLength)
  } yield Block.buildAndSign(1, ts, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), txs, signer)

  private def checkBlock(response: JsValue, expected: Block): Unit = {
    (response \ "version").asOpt[Int].isDefined shouldBe true
    (response \ "timestamp").as[Long] should be >= 0L
    (response \ "reference").asOpt[String].isDefined shouldBe true
    (response \ "transactions" \\ "fee").toList.size should be >= 0
    (response \ "generator").asOpt[String].isDefined shouldBe true
    (response \ "signature").asOpt[String].isDefined shouldBe true
    (response \ "blocksize").as[Int] should be > 0
  }

  routePath("/at/{height}") in {
    val g = for {
      h <- Gen.posNum[Int]
      b <- blockGen
    } yield (h, b)

    forAll(g) {
      case (height, block) =>
        (history.blockAt _).expects(height).returning(Some(block)).once()

        Get(routePath(s"/at/$height")) ~> route ~> check {
          checkBlock(responseAs[JsValue], block)
        }
    }
  }

  routePath("/first") in forAll(blockGen) { block =>
    (history.genesis _).expects().returning(block).once()

    Get(routePath("/first")) ~> route ~> check {
      checkBlock(responseAs[JsObject], block)
    }
  }

  routePath("/seq/{from}/{to}") in {
    val g = for {
      start <- Gen.posNum[Int]
      end <- Gen.choose(start, start + 10)
      blocks <- Gen.listOfN(end - start + 1, blockGen)
    } yield (start, end, blocks)

    forAll(g) {
      case (start, end, blocks) =>
        inSequence {
          for (i <- start to end) {
            (history.blockAt _).expects(i).returning(Some(blocks(i - start))).once()
          }
        }

        Get(routePath("/seq/1/1000")) ~> route should produce (TooBigArrayAllocation)
        Get(routePath(s"/seq/$start/$end")) ~> route ~> check {
          val responseBlocks = responseAs[Seq[JsValue]]

          responseBlocks.length shouldBe blocks.length
        }
    }
  }

  routePath("/last") in forAll(blockGen) { block =>
    inSequence {
      (history.lastBlock _).expects().returning(block).once()
      (history.heightOf(_: Block)).expects(block).returning(Some(1)).once()
    }

    Get(routePath("/last")) ~> route ~> check {
      checkBlock(responseAs[JsObject], block)
    }
  }

  routePath("/height") in forAll(Gen.posNum[Int]) { height =>
    (history.height _).expects().returning(height).once()

    Get(routePath("/height")) ~> route ~> check {
      (responseAs[JsObject] \ "height").as[Int] shouldBe height
    }
  }

  routePath("/height/{signature}") in forAll(blockGen) { block =>
    def sameSignature(s: Array[Byte]) = s.sameElements(block.signerData.signature)
    inSequence {
      (history.blockById(_: Block.BlockId)).expects(where(sameSignature _)).returning(Some(block)).once()
      (history.heightOf(_: Block)).expects(block).returning(Some(10)).once()
    }
    Get(routePath(s"/height/${Base58.encode(block.signerData.signature)}")) ~> route ~> check {
      (responseAs[JsValue] \ "height").as[Int] shouldBe 10
    }
  }

  routePath("/address/{address}/{from}/{to}") in pending
  routePath("/child/{signature}") in pending
  routePath("/delay/{signature}/{blockNum}") in pending
  routePath("/signature/{signature}") in pending
  routePath("/checkpoint") in {
    // todo: invalid checkpoint signature
    // todo: invalid json
    // todo: accepted checkpoint

    probe.expectMsg(BroadcastCheckpoint(???))
  }
}
