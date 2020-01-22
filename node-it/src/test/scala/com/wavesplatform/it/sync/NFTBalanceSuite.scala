package com.wavesplatform.it.sync

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils._
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.BaseTransactionSuiteLike
import com.wavesplatform.it.util._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.{IssueTransaction, IssueTransactionV1}
import com.wavesplatform.transaction.transfer.TransferTransactionV1
import org.scalatest.FreeSpec
import play.api.libs.json._

import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class NFTBalanceSuite extends FreeSpec with BaseTransactionSuiteLike {
  import NFTBalanceSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  override protected def nodes: Seq[Node] = theNodes()

  private val node: Node = nodes.head

  private val issuer: KeyPair = KeyPair("issuer#1".getBytes("UTF-8"))

  private val (simple, nft) = fillPortfolio(issuer, 100, 100)

  private val randomTokenToTransfer = IssuedAsset(nft(Random.nextInt(nft.length)).assetId)

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val fundAndIssue =
      for {
        _      <- traverse(nodes)(_.waitForHeight(2))
        fundTx <- node.transfer(node.address, issuer.stringRepr, 1000.waves, 0.001.waves)
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
      val expectedIds = simple map (_.assetId.base58)

      val assertion =
        getPortfolio(node, issuer.stringRepr) map { ids =>
          ids.toSet shouldBe expectedIds.toSet
        }

      Await.result(assertion, 10.seconds)
    }

    "returns issue transactions on /nft/{address}/limit/{limit}" in {
      val expectedIds = nft.map(_.assetId.base58)

      val assertion =
        getNFTPage(node, issuer.stringRepr, 1000, None) map { ids =>
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
        _          <- node.waitForHeightArise
        issuerNFTs <- getNFTPage(node, issuer.stringRepr, 1000, None)
        otherNFTs  <- getNFTPage(node, other.stringRepr, 1000, None)
      } yield {
        issuerNFTs shouldNot contain(randomTokenToTransfer.id.base58)
        otherNFTs should contain(randomTokenToTransfer.id.base58)
      }

      Await.result(assertion, 50.seconds)
    }
  }

  "pagination" - {
    "works" in {
      val expectedIds = nft
        .filter(_.assetId != randomTokenToTransfer.id)
        .map(_.assetId.base58)
        .toSet

      val assertion = for {
        pagedIds    <- getNFTPaged(node, issuer.stringRepr, 10).map(_.toSet)
        nonPagedIds <- getNFTPage(node, issuer.stringRepr, 1000, None).map(_.toSet)
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
      val assertion = getNFTPage(node, issuer.stringRepr, 10000000, None)
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
      val assertion = getNFTPage(node, issuer.stringRepr, 100, Some("wr0ngbase58str1ng"))
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
  import scala.concurrent.ExecutionContext.Implicits.global

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
    def loop(lastId: Option[String], acc: List[String]): Future[List[String]] = {
      getNFTPage(node, address, limit, lastId) flatMap { ids =>
        if (ids.nonEmpty) loop(ids.lastOption, ids ++ acc)
        else Future.successful(acc)
      }
    }

    loop(None, Nil)
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
