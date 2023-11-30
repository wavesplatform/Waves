package com.wavesplatform.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Accept
import akka.http.scaladsl.server.Route
import com.wavesplatform.TestWallet
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.api.common.CommonBlocksApi
import com.wavesplatform.api.http.ApiError.TooBigArrayAllocation
import com.wavesplatform.api.http.{BlocksApiRoute, CustomJson, RouteTimeout}
import com.wavesplatform.block.serialization.BlockHeaderSerializer
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.{BlockRewardCalculator, Blockchain}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.transaction.assets.exchange.{Order, OrderType}
import com.wavesplatform.utils.{SharedSchedulerMixin, SystemTime}
import monix.reactive.Observable
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.Assertion
import play.api.libs.json.*

import scala.concurrent.duration.*
import scala.util.Random

class BlocksApiRouteSpec
    extends RouteSpec("/blocks")
    with PathMockFactory
    with RestAPISettingsHelper
    with TestWallet
    with WithDomain
    with SharedSchedulerMixin {
  private val blocksApi = mock[CommonBlocksApi]
  private val blocksApiRoute: BlocksApiRoute =
    BlocksApiRoute(restAPISettings, blocksApi, SystemTime, new RouteTimeout(60.seconds)(sharedScheduler))
  private val route = blocksApiRoute.route

  private val testBlock1 = TestBlock.create(Nil).block
  private val testBlock2 = TestBlock.create(Nil, Block.ProtoBlockVersion).block

  private val testBlock1Json = testBlock1.json() ++ Json.obj("height" -> 1, "totalFee" -> 0L)
  private val testBlock2Json = testBlock2.json() ++ Json.obj(
    "height"       -> 2,
    "totalFee"     -> 0L,
    "reward"       -> 5,
    "rewardShares" -> Json.obj(testBlock2.header.generator.toAddress.toString -> 5),
    "VRF"          -> testBlock2.id().toString
  )

  private val testBlock1HeaderJson = BlockHeaderSerializer.toJson(testBlock1.header, testBlock1.bytes().length, 0, testBlock1.signature) ++ Json.obj(
    "height"   -> 1,
    "totalFee" -> 0L
  )

  private val testBlock2HeaderJson = BlockHeaderSerializer.toJson(testBlock2.header, testBlock2.bytes().length, 0, testBlock2.signature) ++ Json.obj(
    "height"       -> 2,
    "totalFee"     -> 0L,
    "reward"       -> 5,
    "rewardShares" -> Json.obj(testBlock2.header.generator.toAddress.toString -> 5),
    "VRF"          -> testBlock2.id().toString
  )

  private val testBlock1Meta = BlockMeta.fromBlock(testBlock1, 1, 0L, None, None)
  private val testBlock2Meta =
    BlockMeta.fromBlock(testBlock2, 2, 0L, Some(5), Some(testBlock2.id())).copy(rewardShares = Seq(testBlock2.header.generator.toAddress -> 5))

  private val invalidBlockId = ByteStr(new Array[Byte](32))
  (blocksApi.block _).expects(invalidBlockId).returning(None).anyNumberOfTimes()
  (blocksApi.meta _).expects(invalidBlockId).returning(None).anyNumberOfTimes()

  routePath("/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }
  }

  routePath("/at/{height}") in {
    (blocksApi.blockAtHeight _).expects(1).returning(Some(testBlock1Meta -> Seq.empty)).once()
    Get(routePath("/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }

    (blocksApi.blockAtHeight _).expects(2).returning(Some(testBlock2Meta -> Seq.empty)).once()
    Get(routePath("/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }

    (blocksApi.blockAtHeight _).expects(3).returning(None).once()
    Get(routePath("/at/3")) ~> route ~> check {
      response.status shouldBe StatusCodes.NotFound
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/{id}") in {
    (blocksApi.block _).expects(testBlock1.id()).returning(Some(testBlock1Meta -> Seq.empty)).once()
    (blocksApi.block _).expects(testBlock2.id()).returning(Some(testBlock2Meta -> Seq.empty)).once()

    Get(routePath(s"/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1Json
    }

    Get(routePath(s"/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2Json
    }

    Get(routePath(s"/$invalidBlockId")) ~> route ~> check {
      response.status.isFailure() shouldBe true
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/seq/{from}/{to}") in {
    (blocksApi
      .blocksRange(_: Int, _: Int))
      .expects(1, 2)
      .returning(
        Observable.fromIterable(
          Seq(
            testBlock1Meta -> Seq.empty,
            testBlock2Meta -> Seq.empty
          )
        )
      )
    Get(routePath("/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1Json, testBlock2Json)
    }
  }

  routePath("/headers/last") in {
    (() => blocksApi.currentHeight).expects().returning(2).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()
    Get(routePath("/headers/last")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
    }
  }

  routePath("/headers/{id}") in {
    (blocksApi.meta _).expects(testBlock1.id()).returning(Some(testBlock1Meta)).once()
    (blocksApi.meta _).expects(testBlock2.id()).returning(Some(testBlock2Meta)).once()

    Get(routePath(s"/headers/${testBlock1.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1HeaderJson
      response
    }

    Get(routePath(s"/headers/${testBlock2.id()}")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
    }

    Get(routePath(s"/headers/$invalidBlockId")) ~> route ~> check {
      response.status.isFailure() shouldBe true
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/headers/at/{height}") in {
    (blocksApi.metaAtHeight _).expects(1).returning(Some(testBlock1Meta)).once()
    (blocksApi.metaAtHeight _).expects(2).returning(Some(testBlock2Meta)).once()
    (blocksApi.metaAtHeight _).expects(3).returning(None).once()

    Get(routePath("/headers/at/1")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock1HeaderJson
    }

    Get(routePath("/headers/at/2")) ~> route ~> check {
      val response = responseAs[JsObject]
      response shouldBe testBlock2HeaderJson
    }

    Get(routePath("/headers/at/3")) ~> route ~> check {
      response.status shouldBe StatusCodes.NotFound
      responseAs[String] should include("block does not exist")
    }
  }

  routePath("/headers/seq/{from}/{to}") in {
    (blocksApi.metaRange _)
      .expects(1, 2)
      .returning(
        Observable.fromIterable(
          Seq(
            testBlock1Meta,
            testBlock2Meta
          )
        )
      )
    Get(routePath("/headers/seq/1/2")) ~> route ~> check {
      val response = responseAs[Seq[JsObject]]
      response shouldBe Seq(testBlock1HeaderJson, testBlock2HeaderJson)
    }
  }

  routePath("/delay/{blockId}/{number}") in {
    val blocks = Vector(
      Block(
        BlockHeader(1, 0, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty, None, None),
        ByteStr(Random.nextBytes(64)),
        Nil
      ),
      Block(
        BlockHeader(1, 1000, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty, None, None),
        ByteStr(Random.nextBytes(64)),
        Nil
      ),
      Block(
        BlockHeader(1, 2000, ByteStr.empty, 0, ByteStr.empty, TxHelpers.defaultSigner.publicKey, Nil, 0, ByteStr.empty, None, None),
        ByteStr(Random.nextBytes(64)),
        Nil
      )
    )

    val blockchain = stub[Blockchain]
    (blockchain.heightOf _).when(blocks.last.id()).returning(Some(3))

    def metaAt(height: Int): Option[BlockMeta] =
      if (height >= 1 && height <= 3)
        Some(BlockMeta(blocks(height - 1).header, ByteStr.empty, None, 1, 0, 0, 0, None, Seq.empty, None))
      else None

    val blocksApi = CommonBlocksApi(blockchain, metaAt, _ => None)
    val route     = blocksApiRoute.copy(commonApi = blocksApi).route
    Get(routePath(s"/delay/${blocks.last.id()}/3")) ~> route ~> check {
      val delay = (responseAs[JsObject] \ "delay").as[Int]
      delay shouldBe 1000
    }

    Get(routePath(s"/delay/${blocks.last.id()}/1")) ~> route ~> check {
      val delay = (responseAs[JsObject] \ "delay").as[Int]
      delay shouldBe 1000
    }

    Get(routePath(s"/delay/${blocks.last.id()}/${BlocksApiRoute.MaxBlocksForDelay}")) ~> route ~> check {
      response.status shouldBe StatusCodes.OK
      val delay = (responseAs[JsObject] \ "delay").as[Int]
      delay shouldBe 1000
    }

    Get(routePath(s"/delay/${blocks.last.id()}/${BlocksApiRoute.MaxBlocksForDelay + 1}")) ~> route ~> check {
      response.status shouldBe StatusCodes.BadRequest
      (responseAs[JsObject] \ "message").as[String] shouldBe TooBigArrayAllocation(BlocksApiRoute.MaxBlocksForDelay).message
    }
  }

  routePath("/heightByTimestamp") - {
    def emulateBlocks(blocks: IndexedSeq[Block]): CommonBlocksApi = {
      require(blocks.nonEmpty)
      val blocksApi = stub[CommonBlocksApi]
      (() => blocksApi.currentHeight).when().returning(blocks.length)
      (blocksApi.metaAtHeight _)
        .when(*)
        .onCall { (height: Int) =>
          if (height < 1 || height > blocks.size) None
          else {
            val block = blocks(height - 1)
            Some(BlockMeta(block.header, block.signature, None, height, 1, 0, 0L, None, Seq.empty, None))
          }
        }
      blocksApi
    }

    "missing blocks" in {
      (blocksApi.metaAtHeight _).expects(1).returning(None).repeat(2)
      (() => blocksApi.currentHeight).expects().returning(5).repeat(2)
      Get(routePath(s"/heightByTimestamp/1")) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.parse("{\"error\":199,\"message\":\"State was altered\"}")
      }
    }

    "ideal blocks" in {
      val blocks = (1 to 10).map(i => TestBlock.create(i * 10, Nil).block)
      val route  = blocksApiRoute.copy(commonApi = emulateBlocks(blocks)).route

      Get(routePath(s"/heightByTimestamp/10")) ~> route ~> check {
        val result = (responseAs[JsObject] \ "height").as[Int]
        result shouldBe 1
      }

      Get(routePath(s"/heightByTimestamp/100")) ~> route ~> check {
        val result = (responseAs[JsObject] \ "height").as[Int]
        result shouldBe 10
      }

      Get(routePath(s"/heightByTimestamp/55")) ~> route ~> check {
        val result = (responseAs[JsObject] \ "height").as[Int]
        result shouldBe 5
      }

      Get(routePath(s"/heightByTimestamp/99")) ~> route ~> check {
        val result = (responseAs[JsObject] \ "height").as[Int]
        result shouldBe 9
      }

      Get(routePath(s"/heightByTimestamp/110")) ~> route ~> check {
        val result = (responseAs[JsObject] \ "height").as[Int]
        result shouldBe 10
      }

      Get(routePath(s"/heightByTimestamp/9")) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.parse("{\"error\":199,\"message\":\"Indicated timestamp is before the start of the blockchain\"}")
      }

      Get(routePath(s"/heightByTimestamp/${System.currentTimeMillis() + 10000}")) ~> route ~> check {
        responseAs[JsObject] shouldBe Json.parse("{\"error\":199,\"message\":\"Indicated timestamp belongs to the future\"}")
      }
    }

    "random blocks" in {
      val (_, blocks) = (1 to 10).foldLeft((0L, Vector.empty[Block])) { case ((ts, blocks), _) =>
        val newBlock = TestBlock.create(ts + 100 + Random.nextInt(10000), Nil).block
        (newBlock.header.timestamp, blocks :+ newBlock)
      }

      val route = blocksApiRoute.copy(commonApi = emulateBlocks(blocks)).route

      blocks.zipWithIndex.foreach { case (block, index) =>
        Get(routePath(s"/heightByTimestamp/${block.header.timestamp}")) ~> route ~> check {
          val result = (responseAs[JsObject] \ "height").as[Int]
          result shouldBe (index + 1)
        }
      }
    }
  }

  "NODE-968. Blocks API should return correct data for orders with attachment" in {
    def checkOrderAttachment(blockInfo: JsObject, expectedAttachment: ByteStr): Assertion = {
      implicit val byteStrFormat: Format[ByteStr] = com.wavesplatform.utils.byteStrFormat
      ((blockInfo \ "transactions").as[JsArray].value.head.as[JsObject] \ "order1" \ "attachment").asOpt[ByteStr] shouldBe Some(expectedAttachment)
    }

    val sender = TxHelpers.signer(1)
    val issuer = TxHelpers.signer(2)
    withDomain(TransactionStateSnapshot, balances = AddrWithBalance.enoughBalances(sender, issuer)) { d =>
      val attachment = ByteStr.fill(32)(1)
      val issue      = TxHelpers.issue(issuer)
      val exchange =
        TxHelpers.exchangeFromOrders(
          TxHelpers.order(OrderType.BUY, Waves, issue.asset, version = Order.V4, attachment = Some(attachment)),
          TxHelpers.order(OrderType.SELL, Waves, issue.asset, version = Order.V4, sender = issuer),
          version = TxVersion.V3
        )

      d.appendBlock(issue)
      val exchangeBlock = d.appendBlock(exchange)

      val route = new BlocksApiRoute(
        d.settings.restAPISettings,
        d.blocksApi,
        SystemTime,
        new RouteTimeout(60.seconds)(sharedScheduler)
      ).route

      Get("/blocks/last") ~> route ~> check {
        checkOrderAttachment(responseAs[JsObject], attachment)
      }

      d.liquidAndSolidAssert { () =>
        Get(s"/blocks/at/3") ~> route ~> check {
          checkOrderAttachment(responseAs[JsObject], attachment)
        }

        Get(s"/blocks/seq/3/3") ~> route ~> check {
          checkOrderAttachment(responseAs[JsArray].value.head.as[JsObject], attachment)
        }

        Get(s"/blocks/address/${exchangeBlock.sender.toAddress}/3/3") ~> route ~> check {
          checkOrderAttachment(responseAs[JsArray].value.head.as[JsObject], attachment)
        }

        Get(s"/blocks/${exchangeBlock.id()}") ~> route ~> check {
          checkOrderAttachment(responseAs[JsObject], attachment)
        }
      }
    }
  }

  "NODE-857. Block meta response should contain correct rewardShares field" in {
    val daoAddress        = TxHelpers.address(3)
    val xtnBuybackAddress = TxHelpers.address(4)

    val settings = DomainPresets.ConsensusImprovements
    val settingsWithFeatures = settings
      .copy(blockchainSettings =
        settings.blockchainSettings.copy(
          functionalitySettings = settings.blockchainSettings.functionalitySettings
            .copy(daoAddress = Some(daoAddress.toString), xtnBuybackAddress = Some(xtnBuybackAddress.toString), xtnBuybackRewardPeriod = 1),
          rewardsSettings = settings.blockchainSettings.rewardsSettings.copy(initial = BlockRewardCalculator.FullRewardInit + 1.waves)
        )
      )
      .setFeaturesHeight(
        BlockchainFeatures.BlockRewardDistribution -> 3,
        BlockchainFeatures.CappedReward            -> 4,
        BlockchainFeatures.CeaseXtnBuyback         -> 5
      )

    withDomain(settingsWithFeatures) { d =>
      val route = new BlocksApiRoute(d.settings.restAPISettings, d.blocksApi, SystemTime, new RouteTimeout(60.seconds)(sharedScheduler)).route

      val miner = d.appendBlock().sender.toAddress

      // BlockRewardDistribution activated
      val configAddrReward3 = d.blockchain.settings.rewardsSettings.initial / 3
      val minerReward3      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward3

      // CappedReward activated
      val configAddrReward4 = BlockRewardCalculator.MaxAddressReward
      val minerReward4      = d.blockchain.settings.rewardsSettings.initial - 2 * configAddrReward4

      // CeaseXTNBuyback activated with expired XTN buyback reward period
      val configAddrReward5 = BlockRewardCalculator.MaxAddressReward
      val minerReward5      = d.blockchain.settings.rewardsSettings.initial - configAddrReward5

      val heightToResult = Map(
        2 -> Map(miner.toString -> d.blockchain.settings.rewardsSettings.initial),
        3 -> Map(miner.toString -> minerReward3, daoAddress.toString -> configAddrReward3, xtnBuybackAddress.toString -> configAddrReward3),
        4 -> Map(miner.toString -> minerReward4, daoAddress.toString -> configAddrReward4, xtnBuybackAddress.toString -> configAddrReward4),
        5 -> Map(miner.toString -> minerReward5, daoAddress.toString -> configAddrReward5)
      )

      val heightToBlock = (2 to 5).map { h =>
        val block = d.appendBlock()

        Seq(true, false).foreach { lsf =>
          checkRewardSharesBlock(route, "/last", heightToResult(h), lsf)
          checkRewardSharesBlock(route, "/headers/last", heightToResult(h), lsf)
        }

        h -> block
      }.toMap
      d.appendBlock()

      Seq(true, false).foreach { lsf =>
        heightToResult.foreach { case (h, expectedResult) =>
          checkRewardSharesBlock(route, s"/at/$h", expectedResult, lsf)
          checkRewardSharesBlock(route, s"/headers/at/$h", expectedResult, lsf)
          checkRewardSharesBlock(route, s"/headers/${heightToBlock(h).id()}", expectedResult, lsf)
          checkRewardSharesBlock(route, s"/${heightToBlock(h).id()}", expectedResult, lsf)
        }
        checkRewardSharesBlockSeq(route, "/seq", 2, 5, heightToResult, lsf)
        checkRewardSharesBlockSeq(route, "/headers/seq", 2, 5, heightToResult, lsf)
        checkRewardSharesBlockSeq(route, s"/address/${miner.toString}", 2, 5, heightToResult, lsf)
      }
    }
  }

  private def checkRewardSharesBlock(route: Route, path: String, expected: Map[String, Long], largeSignificandFormat: Boolean) = {
    val maybeWithLsf =
      if (largeSignificandFormat)
        Get(routePath(path)) ~> Accept(CustomJson.jsonWithNumbersAsStrings)
      else Get(routePath(path))

    maybeWithLsf ~> route ~> check {
      (responseAs[JsObject] \ "rewardShares")
        .as[JsObject]
        .value
        .view
        .mapValues { rewardJson => if (largeSignificandFormat) rewardJson.as[String].toLong else rewardJson.as[Long] }
        .toMap shouldBe expected
    }
  }

  private def checkRewardSharesBlockSeq(
      route: Route,
      prefix: String,
      start: Int,
      end: Int,
      heightToResult: Map[Int, Map[String, Long]],
      largeSignificandFormat: Boolean
  ) = {
    val maybeWithLsf =
      if (largeSignificandFormat)
        Get(routePath(s"$prefix/$start/$end")) ~> Accept(CustomJson.jsonWithNumbersAsStrings)
      else Get(routePath(s"$prefix/$start/$end"))
    maybeWithLsf ~> route ~> check {
      responseAs[Seq[JsObject]]
        .zip(start to end)
        .map { case (obj, h) =>
          h -> (obj \ "rewardShares")
            .as[JsObject]
            .value
            .view
            .mapValues { rewardJson => if (largeSignificandFormat) rewardJson.as[String].toLong else rewardJson.as[Long] }
            .toMap
        }
        .toMap shouldBe heightToResult
    }
  }
}
