package com.wavesplatform.http

import com.typesafe.config.ConfigFactory
import com.wavesplatform.BlockGen
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{CheckpointsSettings, RestAPISettings}
import com.wavesplatform.state2.ByteStr
import org.scalacheck.{Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.DoNotDiscover
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http._
import scorex.block.Block
import scorex.transaction.History

@DoNotDiscover
class BlocksRouteSpec extends RouteSpec("/blocks") with MockFactory with BlockGen with PropertyChecks {

  import BlocksRouteSpec._

  private val config = ConfigFactory.load()
  private val restSettings = RestAPISettings.fromConfig(config)
  private val checkpointSettings = CheckpointsSettings.fromConfig(config)
  private val history = mock[History]
  private val route = BlocksApiRoute(restSettings, checkpointSettings, history, mockWriteToChannel).route

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private def mockWriteToChannel(checkpoint: com.wavesplatform.network.Checkpoint): Unit = {}

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
      b <- randomSignerBlockGen
    } yield (h, b)

    forAll(g) {
      case (height, block) =>
        (history.blockBytes _).expects(height).returning(Some(block.bytes)).once()

        Get(routePath(s"/at/$height")) ~> route ~> check {
          checkBlock(responseAs[JsValue], block)
        }
    }
  }

  routePath("/first") in forAll(randomSignerBlockGen) { block =>
    (history.blockBytes _).expects(1).returning(Some(block.bytes)).once()

    Get(routePath("/first")) ~> route ~> check {
      checkBlock(responseAs[JsObject], block)
    }
  }

  routePath("/seq/{from}/{to}") in {
    // todo: check to <= from
    // todo: check invalid from/to (not numeric)
    forAll(randomBlocksSeqGen) {
      case (start, end, blocks) =>
        inSequence {
          for (i <- start to end) {
            (history.blockBytes _).expects(i).returning(if (i - start < blocks.length) Some(blocks(i - start).bytes) else None).once()
          }
        }

        Get(routePath("/seq/1/1000")) ~> route should produce(TooBigArrayAllocation)
        Get(routePath(s"/seq/$start/$end")) ~> route ~> check {
          val responseBlocks = responseAs[Seq[JsValue]]

          responseBlocks.length shouldBe blocks.length
        }
    }
  }

  routePath("/last") in forAll(randomSignerBlockGen, positiveIntGen) { case (block, h) =>
    (history.height _).expects().returning(h).anyNumberOfTimes()
    (history.blockBytes _).expects(h).returning(Some(block.bytes)).anyNumberOfTimes()
    (history.heightOf(_: ByteStr)).expects(block.uniqueId).returning(Some(h)).anyNumberOfTimes()

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
    forAll(randomSignerBlockGen) { block =>
      (history.heightOf(_: ByteStr)).expects(block.uniqueId).returning(Some(10)).anyNumberOfTimes()
      (history.blockBytes _).expects(10).returning(Some(block.bytes)).anyNumberOfTimes()
      Get(routePath(s"/height/${block.uniqueId.base58}")) ~> route ~> check {
        (responseAs[JsValue] \ "height").as[Int] shouldBe 10
      }
    }
  }

  routePath("/address/{address}/{from}/{to}") in {
    // todo: check incorrect address
    // todo: check if to <= from
    // todo: check empty block sequence

    forAll(mixedBlocksSeqGen) {
      case (start, end, blocks) =>
        inSequence {
          for (i <- start to end) {
            (history.blockBytes _).expects(i).returning(if (i - start < blocks.length) Some(blocks(i - start).bytes) else None).once()
          }
        }

        Get(routePath(s"/address/${BlockGen.predefinedSignerPrivateKey.address}/1/1000")) ~> route should produce(TooBigArrayAllocation)

        Get(routePath(s"/address/${BlockGen.predefinedSignerPrivateKey.address}/$start/$end")) ~> route ~> check {
          val responseBlocks = responseAs[Seq[JsValue]]
          val zeroSignerBlocks = blocks.filter(_.signerData.generator == PublicKeyAccount(BlockGen.predefinedSignerPrivateKey.publicKey))
          responseBlocks.length shouldBe zeroSignerBlocks.length
        }
    }
  }

  routePath("/child/{signature}") ignore {
    // todo: check block not found (404?)
    // todo: check invalid signature
    forAll(randomSignerBlockGen, randomSignerBlockGen, positiveIntGen) { case (block1, block2, h1) =>

      (history.blockBytes _).expects(h1).returns(Some(block1.bytes)).anyNumberOfTimes()
      (history.heightOf _).expects(block1.uniqueId).returns(Some(h1)).anyNumberOfTimes()

      (history.blockBytes _).expects(h1 + 1).returns(Some(block2.bytes)).anyNumberOfTimes()
      (history.heightOf _).expects(block2.uniqueId).returns(Some(h1 + 1)).anyNumberOfTimes()


      Get(routePath(s"/child/${block1.uniqueId.base58}")) ~> route ~> check {
        checkBlock(responseAs[JsValue], block2)
      }
    }
  }

  routePath("/delay/{signature}/{blockNum}") in {
    forAll(randomSignerBlockGen) { block =>
      inSequence {
      }
    }
  }

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