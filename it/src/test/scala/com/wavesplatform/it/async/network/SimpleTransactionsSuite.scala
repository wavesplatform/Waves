package com.wavesplatform.it.network

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.AsyncNetworkApi._
import com.wavesplatform.it.api._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.network.{RawBytes, TransactionSpec}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.account.Address
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.language.postfixOps

class SimpleTransactionsSuite extends BaseTransactionSuite with Matchers with ScalaFutures
  with IntegrationPatience with RecoverMethods with RequestErrorAssert {

  private val waitCompletion = 2.minutes

  override protected def nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(_.quorum(2))
    .withDefault(3)
    .build()

  private def node = nodes.head

  test("valid tx send by network to node should be in blockchain") {
    val tx = TransferTransaction.create(None,
      node.privateKey,
      Address.fromString(node.address).right.get,
      1L,
      System.currentTimeMillis(),
      None,
      100000L,
      Array()).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, tx.bytes()))
      tx <- node.waitForTransaction(tx.id().base58)
    } yield {
      tx shouldBe Transaction(tx.`type`, tx.id, tx.fee, tx.timestamp)
    }
    Await.result(f, waitCompletion)
  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = TransferTransaction.create(None,
      node.privateKey,
      Address.fromString(node.address).right.get,
      1L,
      System.currentTimeMillis() + (1 days).toMillis,
      None,
      100000L,
      Array()).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, tx.bytes()))
      maxHeight <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(maxHeight + 1))
      _ <- traverse(nodes)(_.ensureTxDoesntExist(tx.id().base58))
    } yield ()
    Await.result(f, waitCompletion)
  }

  test("should blacklist senders of non-parsable transactions") {
    val f = for {
      blacklistBefore <- node.blacklistedPeers
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, "foobar".getBytes(StandardCharsets.UTF_8)))
      _ <- node.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers > ${blacklistBefore.size}")(_.blacklistedPeers, _.lengthCompare(blacklistBefore.size) > 0, 500.millis)
    } yield ()
    Await.result(f, waitCompletion)
  }
}
