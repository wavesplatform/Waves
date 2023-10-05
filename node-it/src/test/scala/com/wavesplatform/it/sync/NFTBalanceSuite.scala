package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.it.*
import com.wavesplatform.it.api.AsyncHttpApi.*
import com.wavesplatform.it.api.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import play.api.libs.json.*

import scala.concurrent.Future.traverse
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.Random

class NFTBalanceSuite extends BaseFreeSpec {
  import NFTBalanceSuite.*

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  private def node: Node = nodes.head

  private val issuer: KeyPair = KeyPair("issuer#1".getBytes)

  private val (simple, nft) = fillPortfolio(issuer, 100, 100)

  private val randomTokenToTransfer = IssuedAsset(nft(Random.nextInt(nft.length)).assetId)

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val fundAndIssue =
      for {
        _      <- traverse(nodes)(_.waitForHeight(2))
        fundTx <- node.transfer(node.keyPair, issuer.toAddress.toString, 1000.waves, 0.001.waves)
        _      <- node.waitForTransaction(fundTx.id)
        _ <- Future.sequence((simple ++ nft) map { tx =>
          for {
            itx <- node.signedBroadcast(tx.json())
            _   <- node.waitForTransaction(itx.id)
          } yield ()
        })
      } yield ()

    Await.ready(fundAndIssue, 2.minutes)
  }

  "after activation" - {
    "returns only non-nft portfolio on /balance/{address}" in {
      val expectedIds = simple map (_.assetId.toString)

      val assertion =
        getPortfolio(node, issuer.toAddress.toString) map { ids =>
          ids.toSet shouldBe expectedIds.toSet
        }

      Await.result(assertion, 10.seconds)
    }

    "returns issue transactions on /nft/{address}/limit/{limit}" in {
      val expectedIds = nft.map(_.assetId.toString)

      val assertion =
        getNFTPage(node, issuer.toAddress.toString, 1000, None) map { ids =>
          ids.toSet shouldBe expectedIds.toSet
        }

      Await.result(assertion, 10.seconds)
    }

    "returns only nft with balance > 0 on /nft/{address}/limit/{limit}" in {
      val other = KeyPair("other".getBytes)

      val transfer = TransferTransaction
        .selfSigned(1.toByte, issuer, other.toAddress, randomTokenToTransfer, 1, Waves, 0.001.waves, ByteStr.empty, System.currentTimeMillis())
        .explicitGet()

      val assertion = for {
        tx         <- node.signedBroadcast(transfer.json())
        _          <- node.waitForTransaction(tx.id)
        _          <- node.waitForHeightArise
        issuerNFTs <- getNFTPage(node, issuer.toAddress.toString, 1000, None)
        otherNFTs  <- getNFTPage(node, other.toAddress.toString, 1000, None)
      } yield {
        issuerNFTs shouldNot contain(randomTokenToTransfer.id.toString)
        otherNFTs should contain(randomTokenToTransfer.id.toString)
      }

      Await.result(assertion, 50.seconds)
    }
  }

  "pagination" - {
    "works" in {
      val expectedIds = nft
        .filter(_.assetId != randomTokenToTransfer.id)
        .map(_.assetId.toString)
        .toSet

      val assertion = for {
        pagedIds    <- getNFTPaged(node, issuer.toAddress.toString, 10).map(_.toSet)
        nonPagedIds <- getNFTPage(node, issuer.toAddress.toString, 1000, None).map(_.toSet)
      } yield {
        pagedIds shouldBe expectedIds
        nonPagedIds shouldBe expectedIds
      }

      Await.result(
        assertion,
        1.minute
      )
    }

    "returns error on wrong limit" in {
      val assertion = getNFTPage(node, issuer.toAddress.toString, 10000000, None)
        .map(_ => org.scalatest.Assertions.fail("BadRequest expected"))
        .recoverWith { case ex: Throwable =>
          Future.successful {
            assert(ex.getMessage contains "Too big sequence requested")
          }
        }

      Await.result(assertion, 10.seconds)
    }

    "returns error on wrong base58 in after" in {
      val assertion = getNFTPage(node, issuer.toAddress.toString, 100, Some("wr0ngbase58str1ng"))
        .map(_ => org.scalatest.Assertions.fail("BadRequest expected"))
        .recoverWith { case ex: Throwable =>
          Future.successful {
            assert(ex.getMessage contains "Invalid asset id")
          }
        }

      Await.result(assertion, 10.seconds)
    }
  }

}

object NFTBalanceSuite {
  import scala.concurrent.ExecutionContext.Implicits.global

  def fillPortfolio(issuer: KeyPair, nft: Int, simple: Int): (List[IssueTransaction], List[IssueTransaction]) = {

    val simpleAssets = List.fill[IssueTransaction](simple) {
      IssueTransaction
        .selfSigned(
          TxVersion.V1,
          issuer,
          "SimpleAsset",
          s"Simple Test Asset ${Random.nextInt(1000)}",
          1000,
          8,
          reissuable = true,
          script = None,
          1.waves,
          System.currentTimeMillis()
        )
        .explicitGet()
    }

    val nonFungibleAssets = List.fill[IssueTransaction](nft) {
      IssueTransaction
        .selfSigned(
          TxVersion.V1,
          issuer,
          "NonFungibleAsset",
          s"NFT Test Asset ${Random.nextInt(1000)}",
          1,
          0,
          reissuable = false,
          script = None,
          1.waves,
          System.currentTimeMillis()
        )
        .explicitGet()
    }

    (simpleAssets, nonFungibleAssets)
  }

  def fundAddresses(faucet: Node, addrs: String*): Unit = {
    import com.wavesplatform.it.api.AsyncHttpApi.*

    val transactions =
      Future.sequence(addrs map { addr =>
        NodeAsyncHttpApi(faucet)
          .transfer(faucet.keyPair, addr, 1000.waves, 0.001.waves)
          .flatMap { tx =>
            NodeAsyncHttpApi(faucet)
              .waitForTransaction(tx.id, retryInterval = 1.second)
          }
      })

    Await.ready(transactions, 30.seconds)
  }

  // returns first page of asset ids from addresses portfolio
  // obtained via paged api
  def getNFTPage(node: Node, issuer: String, limit: Int, maybeAfter: Option[String]): Future[List[String]] = {
    val afterParam = maybeAfter.fold("")(id => s"?after=$id")

    node
      .get(s"/assets/nft/$issuer/limit/$limit$afterParam")
      .as[JsArray]
      .map { arr =>
        arr.value.map { json =>
          (json \ "assetId").as[String]
        }.toList
      }
  }

  // returns asset ids from addresses portfolio
  // obtained via paged api
  def getNFTPaged(node: Node, address: String, limit: Int): Future[List[String]] = {
    def loop(lastId: Option[String], acc: List[String]): Future[List[String]] = {
      getNFTPage(node, address, limit, lastId) flatMap { ids =>
        if (ids.nonEmpty) loop(ids.lastOption, ids ++ acc)
        else Future.successful(acc)
      }
    }

    loop(None, Nil)
  }

  // returns asset ids from addresses portfolio
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
