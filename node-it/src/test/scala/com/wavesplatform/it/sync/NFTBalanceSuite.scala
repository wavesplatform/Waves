package com.wavesplatform.it.sync

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils._
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.NodesFromDocker
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV1}
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec}
import play.api.libs.json._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class NFTBalanceSuite
    extends FreeSpec
    with WaitForHeight2
    with IntegrationSuiteWithThreeAddresses
    with BeforeAndAfterAll
    with NodesFromDocker
    with CancelAfterFailure {

  import NFTBalanceSuite._

  override protected def nodeConfigs: Seq[Config] = configs

  protected val theNodes: Coeval[Seq[Node]] = Coeval.evalOnce {
    Option(System.getProperty("waves.it.config.file")) match {
      case None => dockerNodes()
      case Some(filePath) =>
        val defaultConfig = ConfigFactory.load()
        ConfigFactory
          .parseFile(new File(filePath))
          .getConfigList("nodes")
          .asScala
          .map(cfg => new ExternalNode(cfg.withFallback(defaultConfig).resolve()))
    }
  }

  override protected def nodes: Seq[Node] = theNodes()

  private val node: Node = nodes.head

  private val issuer: KeyPair = KeyPair("issuer#1".getBytes("UTF-8"))

  private val (simple, nft) = fillPortfolio(issuer, 100, 100)

  private val randomTokenToTransfer = IssuedAsset(nft(Random.nextInt(nft.length)).assetId)

  protected override def beforeAll(): Unit = {
    theNodes.run()

    val fundAndIssue =
      for {
        _      <- traverse(nodes)(_.waitForHeight(2))
        fundTx <- node.transfer(node.address, issuer.address, 1000.waves, 0.001.waves)
        _      <- node.waitForTransaction(fundTx.id)
        _ <- Future.sequence((simple ++ nft) map { tx =>
          for {
            itx <- node.signedBroadcast(tx.json())
            _   <- node.waitForTransaction(itx.id)
          } yield ()
        })
      } yield ()

    Await.ready(fundAndIssue, 2.minutes)

    super.beforeAll()
  }

  "after activation" - {
    "returns only non-nft portfolio on /balance/{address}" in {
      val expectedIds = simple map (_.assetId.base58)

      val assertion =
        getPortfolio(node, issuer.address) map { ids =>
          ids.toSet shouldBe expectedIds.toSet
        }

      Await.result(assertion, 10.seconds)
    }

    "returns issue transactions on /nft/{address}/limit/{limit}" in {
      val expectedIds = nft.map(_.assetId.base58)

      val assertion =
        getNFTPage(node, issuer.address, 1000, None) map { ids =>
          ids.toSet shouldBe expectedIds.toSet
        }

      Await.result(assertion, 10.seconds)
    }

    "returns only nft with balance > 0 on /nft/{address}/limit/{limit}" in {
      val other = KeyPair("other".getBytes)

      val transfer = TransferTransactionV1
        .selfSigned(
          randomTokenToTransfer,
          issuer,
          other,
          1,
          System.currentTimeMillis(),
          Waves,
          0.001.waves,
          Array.emptyByteArray
        )
        .explicitGet()

      val assertion = for {
        tx         <- node.signedBroadcast(transfer.json())
        _          <- node.waitForTransaction(tx.id)
        issuerNFTs <- getNFTPage(node, issuer.address, 1000, None)
        otherNFTs  <- getNFTPage(node, other.address, 1000, None)
      } yield {
        issuerNFTs shouldNot contain(randomTokenToTransfer.id.base58)
        otherNFTs should contain(randomTokenToTransfer.id.base58)
      }

      Await.result(assertion, 10.seconds)
    }
  }

  "pagination" - {
    "works" in {
      val expectedIds = nft
        .filter(_.assetId != randomTokenToTransfer.id)
        .map(_.assetId.base58)
        .toSet

      val assertion = for {
        pagedIds    <- getNFTPaged(node, issuer.address, 10).map(_.toSet)
        nonPagedIds <- getNFTPage(node, issuer.address, 1000, None).map(_.toSet)
      } yield {
        pagedIds shouldBe expectedIds
        nonPagedIds shouldBe expectedIds
      }

      Await.result(assertion, 1.minute)
    }

    "returns error on wrong limit" in {
      val assertion = getNFTPage(node, issuer.address, 10000000, None)
        .map(_ => org.scalatest.Assertions.fail("BadRequest expected"))
        .recoverWith {
          case ex: Throwable =>
            Future.successful {
              assert(ex.getMessage contains "Too big sequences requested")
            }
        }

      Await.result(assertion, 10.seconds)
    }

    "returns error on wrong base58 in after" in {
      val assertion = getNFTPage(node, issuer.address, 100, Some("wr0ngbase58str1ng"))
        .map(_ => org.scalatest.Assertions.fail("BadRequest expected"))
        .recoverWith {
          case ex: Throwable =>
            Future.successful {
              assert(ex.getMessage contains "Unable to decode asset id")
            }
        }

      Await.result(assertion, 10.seconds)
    }
  }

}

object NFTBalanceSuite {
  val configs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .overrideBase(_.raw(s"""
                          |waves.blockchain.custom.functionality.pre-activated-features = {
                          |          2 = 0
                          |          3 = 0
                          |          4 = 0
                          |          5 = 0
                          |          6 = 0
                          |          7 = 0
                          |          9 = 0
                          |          10 = 0
                          |          11 = 0
                          |          12 = 0
                          |          13 = 0
                          |}
         """.stripMargin))
      .buildNonConflicting()

  def fillPortfolio(issuer: KeyPair, nft: Int, simple: Int): (List[IssueTransaction], List[IssueTransaction]) = {

    val simpleAssets = List.fill[IssueTransaction](simple) {
      IssueTransactionV1
        .selfSigned(
          issuer,
          s"SimpleAsset".getBytes("UTF-8"),
          s"Simple Test Asset ${Random.nextInt(1000)}".getBytes("UTF-8"),
          1000,
          8,
          reissuable = true,
          1.waves,
          System.currentTimeMillis()
        )
        .explicitGet()
    }

    val nonFungibleAssets = List.fill[IssueTransaction](nft) {
      IssueTransactionV1
        .selfSigned(
          issuer,
          "NonFungibleAsset".getBytes("UTF-8"),
          s"NFT Test Asset ${Random.nextInt(1000)}".getBytes("UTF-8"),
          1,
          0,
          reissuable = false,
          1.waves,
          System.currentTimeMillis()
        )
        .explicitGet()
    }

    (simpleAssets, nonFungibleAssets)
  }

  def fundAddresses(faucet: Node, addrs: String*): Unit = {
    import com.wavesplatform.it.api.AsyncHttpApi._

    val transactions =
      Future.sequence(addrs map { addr =>
        NodeAsyncHttpApi(faucet)
          .transfer(faucet.address, addr, 1000.waves, 0.001.waves)
          .flatMap { tx =>
            NodeAsyncHttpApi(faucet)
              .waitForTransaction(tx.id, retryInterval = 1.second)
          }
      })

    Await.ready(transactions, 30.seconds)
  }

  // returns first page of issue transactions ids from addresses portfolio
  // obtained via paged api
  def getNFTPage(node: Node, issuer: String, limit: Int, maybeAfter: Option[String]): Future[List[String]] = {
    val afterParam = maybeAfter.fold("")(id => s"?after=$id")

    node
      .get(s"/assets/nft/$issuer/limit/$limit$afterParam")
      .as[JsArray]
      .map { arr =>
        arr.value.map { json =>
          (json \ "id").as[String]
        }.toList
      }
  }

  // returns issue transactions ids from addresses portfolio
  // obtained via paged api
  def getNFTPaged(node: Node, address: String, limit: Int): Future[List[String]] = {
    def loop(lastId: String, acc: List[String]): Future[List[String]] = {
      getNFTPage(node, address, limit, Some(lastId)) flatMap { ids =>
        if (ids.nonEmpty) loop(ids.last, ids ++ acc)
        else Future.successful(acc)
      }
    }

    getNFTPage(node, address, limit, None) flatMap { ids =>
      if (ids.nonEmpty) loop(ids.last, ids)
      else Future.successful(ids)
    }

  }

  //returns asset ids from addresses portfolio
  def getPortfolio(node: Node, address: String): Future[List[String]] = {
    node
      .get(s"/assets/balance/$address")
      .as[JsObject]
      .map { json =>
        (json \ "balances").as[List[String]](Reads.list(Reads { details =>
          (details \ "issueTransaction" \ "assetId").validate[String]
        }))
      }
  }
}
