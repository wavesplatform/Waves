package com.wavesplatform.http

import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.BlockGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{CheckpointsSettings, RestAPISettings}
import com.wavesplatform.state2.EqByteArray
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http._
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.transaction.History

class BlocksRouteSpec extends RouteSpec("/blocks") with MockFactory with BlockGen with PropertyChecks {

  import BlocksRouteSpec._

  private val config = ConfigFactory.load()
  private val restSettings = RestAPISettings.fromConfig(config)
  private val checkpointSettings = CheckpointsSettings.fromConfig(config)
  private val probe = TestProbe()
  private val history = mock[History]
  private val route = BlocksApiRoute(restSettings, checkpointSettings, history, probe.ref).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private val blockSeqGen = for {
    start <- Gen.posNum[Int].label("from")
    end <- Gen.chooseNum(start, start + 20).label("to")
    blockCount <- Gen.choose(0, end - start + 1).label("actualBlockCount")
    blocks <- Gen.listOfN(blockCount, blockGen).label("blocks")
  } yield (start, end, blocks)

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
    // todo: check invalid height
    // todo: check block not found (404?)
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
    (history.blockAt _).expects(1).returning(Some(block)).once()

    Get(routePath("/first")) ~> route ~> check {
      checkBlock(responseAs[JsObject], block)
    }
  }

  routePath("/seq/{from}/{to}") in {
    // todo: check to <= from
    // todo: check invalid from/to (not numeric)
    forAll(blockSeqGen) {
      case (start, end, blocks) =>
        inSequence {
          for (i <- start to end) {
            (history.blockAt _).expects(i).returning(if (i - start < blocks.length) Some(blocks(i - start)) else None).once()
          }
        }

        Get(routePath("/seq/1/1000")) ~> route should produce(TooBigArrayAllocation)
        Get(routePath(s"/seq/$start/$end")) ~> route ~> check {
          val responseBlocks = responseAs[Seq[JsValue]]

          responseBlocks.length shouldBe blocks.length
        }
    }
  }

  routePath("/last") in forAll(blockGen, positiveIntGen) { case (block, h) =>
    (history.height _).expects().returning(h).anyNumberOfTimes()
    (history.blockAt _).expects(h).returning(Some(block)).anyNumberOfTimes()
    (history.heightOf(_: BlockId)).expects(block.uniqueId).returning(Some(h)).anyNumberOfTimes()

    Get(routePath("/last")) ~> route ~> check {
      val response1: JsObject = responseAs[JsObject]
      checkBlock(response1, block)
    }
  }

  routePath("/height") in forAll(Gen.posNum[Int]) { height =>
    (history.height _).expects().returning(height).once()

    Get(routePath("/height")) ~> route ~> check {
      (responseAs[JsObject] \ "height").as[Int] shouldBe height
    }
  }

  routePath("/height/{signature}") in {
    // todo: check invalid signature
    // todo: check block not found (404?)
    forAll(blockGen) { block =>
      (history.heightOf(_: BlockId)).expects(where(sameSignature(block.uniqueId)(_))).returning(Some(10)).anyNumberOfTimes()
      (history.blockAt _).expects(10).returning(Some(block)).anyNumberOfTimes()

      Get(routePath(s"/height/${Base58.encode(block.signerData.signature)}")) ~> route ~> check {
        (responseAs[JsValue] \ "height").as[Int] shouldBe 10
      }
    }
  }

  routePath("/address/{address}/{from}/{to}") in {
    // todo: check incorrect address
    // todo: check if to <= from
    // todo: check empty block sequence
    val g = for {
      account <- accountGen
      blockSeq <- blockSeqGen
    } yield (account, blockSeq)

    forAll(g) { case (address, (from, to, blocks)) =>
      (history.generatedBy _).expects(Account.fromPublicKey(address.publicKey), from, to).returning(blocks).once()
      Get(routePath(s"/address/${address.address}/$from/$to")) ~> route ~> check {
        val response = responseAs[Seq[JsValue]]
        response.length shouldBe blocks.length

        response.zip(blocks).foreach((checkBlock _).tupled)
      }
    }
  }

  routePath("/child/{signature}") in {
    // todo: check block not found (404?)
    // todo: check invalid signature
    forAll(blockGen, blockGen, positiveIntGen) { case (block1, block2, h1) =>

      (history.blockAt _).expects(h1).returns(Some(block1)).anyNumberOfTimes()
      (history.heightOf _).expects(where(sameSignature(block1.uniqueId)(_))).returns(Some(h1)).anyNumberOfTimes()

      (history.blockAt _).expects(h1 + 1).returns(Some(block2)).anyNumberOfTimes()
      (history.heightOf _).expects(where(sameSignature(block2.uniqueId)(_))).returns(Some(h1 + 1)).anyNumberOfTimes()


      Get(routePath(s"/child/${Base58.encode(block1.signerData.signature)}")) ~> route ~> check {
        checkBlock(responseAs[JsValue], block2)
      }
    }
  }

  routePath("/delay/{signature}/{blockNum}") in {
    forAll(blockGen) { block =>
      inSequence {
      }
    }
  }

//  routePath("/signature/{signature}") in {
//    val g = for {
//      block <- blockGen
//      invalidSignature <- byteArrayGen(10)
//    } yield (block, invalidSignature)
//
//    forAll(g) { case (block, invalidSignature) =>
//
//      (history.blockById(_: Block.BlockId)).expects(where(sameSignature(invalidSignature) _)).returning(None).once()
//      (history.heightOf(_: Block.BlockId)).expects(*).returning(Some(1)).noMoreThanTwice()
//
//      Get(routePath(s"/signature/${Base58.encode(block.signerData.signature)}")) ~> route ~> check {
//        checkBlock(responseAs[JsValue], block)
//      }
//
//      Get(routePath(s"/signature/${Base58.encode(invalidSignature)}")) ~> route should produce(BlockNotExists)
//    }
//  }

  routePath("/checkpoint") in pendingUntilFixed {
    Post(routePath("/checkpoint"), Checkpoint("", Seq.empty)) ~> route should produce(WrongJson())

    // todo: invalid json
    // todo: accepted checkpoint
  }
}

object BlocksRouteSpec {

  case class Item(height: Int, signature: String)

  case class Checkpoint(signature: String, items: Seq[Item])

  implicit val itemFormat: Format[Item] = Json.format
  implicit val checkpointFormat: Format[Checkpoint] = Json.format
}