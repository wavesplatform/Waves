package com.wavesplatform.it.network

import com.wavesplatform.it.MatcherTestSuite.Configs
import com.wavesplatform.it._
import com.wavesplatform.it.network.client.RawBytes
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.PaymentTransaction

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


class SimpleTransactionsSuite extends FunSuite with BeforeAndAfterAll with Matchers with ScalaFutures
  with IntegrationPatience with RecoverMethods with RequestErrorAssert with IntegrationNodesInitializationAndStopping
  with IntegrationTestsScheme {

  override val docker = new Docker()

  override val nodes = Configs.take(2).map(docker.startNode)
  val node = nodes.head

  test("valid tx send by network to node should be in blockchain") {
    val tx = PaymentTransaction.create(
      PrivateKeyAccount(Base58.decode(node.accountSeed).get),
      Account.fromString(node.address).right.get,
      1L,
      100000L,
      System.currentTimeMillis()).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(25.toByte, tx.bytes))
      tx <- node.waitForTransaction(tx.id.base58)
    } yield {
      tx shouldBe Node.Transaction(tx.`type`, tx.id, tx.fee, tx.timestamp)
    }
    Await.result(f, 60 seconds)
  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = PaymentTransaction.create(
      PrivateKeyAccount(Base58.decode(node.accountSeed).get),
      Account.fromString(node.address).right.get,
      1L,
      100000L,
      System.currentTimeMillis() + (1 days).toMillis).right.get
    val f = for {
      _ <- node.sendByNetwork(RawBytes(25.toByte, tx.bytes))
      _ <- Future.successful(Thread.sleep(2000))
      _ <- Future.sequence(nodes.map(_.isTransactionNotExists(tx.id.base58)))
    } yield ()
    Await.result(f, 60 seconds)
  }

  override protected def afterAll(): Unit = {
    docker.close()
    super.afterAll()
  }
}
