package com.wavesplatform.it.network

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.wavesplatform.it._
import com.wavesplatform.it.api.AsyncHttpApi._
import com.wavesplatform.it.api.AsyncNetworkApi._
import com.wavesplatform.it.api.Node.{BlacklistedPeer, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.network.{RawBytes, TransactionSpec}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.account.{Address, PrivateKeyAccount}
import scorex.transaction.assets.TransferTransaction

import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
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
      PrivateKeyAccount.fromSeed(node.accountSeed).right.get,
      Address.fromString(node.address).right.get,
      1L,
      System.currentTimeMillis(),
      None,
      100000L,
      Array()).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, tx.bytes()))
      _ <- Future.successful(Thread.sleep(2000))

      height <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(height + 1))
      tx <- node.waitForTransaction(tx.id().base58)
    } yield {
      tx shouldBe Transaction(tx.`type`, tx.id, tx.fee, tx.timestamp)
    }
    Await.result(f, waitCompletion)
  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = TransferTransaction.create(None,
      PrivateKeyAccount.fromSeed(node.accountSeed).right.get,
      Address.fromString(node.address).right.get,
      1L,
      System.currentTimeMillis() + (1 days).toMillis,
      None,
      100000L,
      Array()).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, tx.bytes()))
      _ <- Future.successful(Thread.sleep(2000))
      _ <- Future.sequence(nodes.map(_.ensureTxDoesntExist(tx.id().base58)))
    } yield ()
    Await.result(f, waitCompletion)
  }

  test("should blacklist senders of non-parsable transactions") {
    val f = for {
      blacklistBefore <- node.blacklistedPeers
      _ <- node.sendByNetwork(RawBytes(TransactionSpec.messageCode, "foobar".getBytes(StandardCharsets.UTF_8)))
      _ <- node.waitFor[Seq[BlacklistedPeer]](s"blacklistedPeers > ${blacklistBefore.size}")(_.blacklistedPeers, _.size > blacklistBefore.size, 500.millis)
    } yield ()
    Await.result(f, waitCompletion)
  }
}
