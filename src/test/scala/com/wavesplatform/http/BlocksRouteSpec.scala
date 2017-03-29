package com.wavesplatform.http

import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import com.wavesplatform.http.ApiMarshallers._
import com.wavesplatform.settings.{CheckpointsSettings, RestAPISettings}
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json._
import scorex.account.Account
import scorex.api.http.{BlockNotExists, BlocksApiRoute, TooBigArrayAllocation, WrongJson}
import scorex.block.Block
import scorex.consensus.nxt.{NxtLikeConsensusBlockData, WavesConsensusModule}
import scorex.crypto.encode.Base58
import scorex.transaction.{History, TransactionGen}

class BlocksRouteSpec extends RouteSpec("/blocks") with MockFactory with TransactionGen with PropertyChecks {
  import BlocksRouteSpec._

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
    (history.genesis _).expects().returning(block).once()

    Get(routePath("/first")) ~> route ~> check {
      checkBlock(responseAs[JsObject], block)
    }
  }

  routePath("/seq/{from}/{to}") in {
    // todo: check to <= from
    // todo: check invalid from/to (not numeric)
    val g = for {
      start <- Gen.posNum[Int]
      end <- Gen.choose(start, start + 10)
      blockCount <- Gen.choose(0, end - start + 1)
      blocks <- Gen.listOfN(blockCount, blockGen)
    } yield (start, end, blocks)

    forAll(g) {
      case (start, end, blocks) =>
        inSequence {
          for (i <- start to end) {
            (history.blockAt _).expects(i).returning(if (i - start < blocks.length) Some(blocks(i - start)) else None).once()
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

  private def sameSignature(block: Block)(s: Array[Byte]): Boolean = sameSignature(block.signerData.signature)(s)
  private def sameSignature(signature: Block.BlockId)(s: Array[Byte]): Boolean = s.sameElements(signature)
  private def withBlock(block: Block): Unit =
    (history.blockById(_: Block.BlockId)).expects(where(sameSignature(block) _)).returning(Some(block)).once()

  routePath("/height/{signature}") in {
    // todo: check invalid signature
    // todo: check block not found (404?)
    forAll(blockGen) { block =>
      inSequence {
        withBlock(block)
        (history.heightOf(_: Block)).expects(block).returning(Some(10)).once()
      }
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
      from <- Gen.posNum[Int]
      to <- Gen.choose(from, from + 100)
      account <- accountGen
      blockCount <- Gen.choose(0, to - from)
      blocks <- Gen.listOfN(blockCount, blockGen)
    } yield (account, from, to, blocks)

    forAll(g) { case (address, from, to, blocks) =>
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
    forAll(blockGen) { block =>
      inSequence {
        withBlock(block)
        (history.child _).expects(block).returning(Some(block)).once
      }

      Get(routePath(s"/child/${Base58.encode(block.signerData.signature)}")) ~> route ~> check {
        checkBlock(responseAs[JsValue], block)
      }
    }
  }

  routePath("/delay/{signature}/{blockNum}") in {
    forAll(blockGen) { block =>
      inSequence {
      }
    }
  }

  routePath("/signature/{signature}") in {
    val g = for {
      block <- blockGen
      invalidSignature <- byteArrayGen(10)
    } yield (block, invalidSignature)

    forAll(g) { case (block, invalidSignature) =>
      withBlock(block)
      (history.blockById(_: Block.BlockId)).expects(where(sameSignature(invalidSignature) _)).returning(None).once()
      (history.heightOf(_: Block.BlockId)).expects(*).returning(Some(1)).noMoreThanTwice()

      Get(routePath(s"/signature/${Base58.encode(block.signerData.signature) }")) ~> route ~> check {
        checkBlock(responseAs[JsValue], block)
      }

      Get(routePath(s"/signature/${Base58.encode(invalidSignature) }")) ~> route should produce (BlockNotExists)
    }
  }

  routePath("/checkpoint") in pendingUntilFixed {
    Post(routePath("/checkpoint"), Checkpoint("", Seq.empty)) ~> route should produce (WrongJson())

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