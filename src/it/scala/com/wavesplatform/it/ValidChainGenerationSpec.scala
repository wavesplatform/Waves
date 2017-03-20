package com.wavesplatform.it

import org.scalatest._
import org.scalatest.concurrent.{Eventually, IntegrationPatience, ScalaFutures}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class ValidChainGenerationSpec(allNodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with Eventually {

  "Generate 30 blocks and synchronise" in {
    val addressToNode = allNodes.map(n => n.address -> n).toMap
    var balances = Await.result(Future.traverse(allNodes)(n => n.balance(n.address))
      .map(_.map(b => b.address -> b.balance).toMap), 30.seconds)

    def makeTransfer(): Future[String] = {
      val shuffledAddresses = Random.shuffle(addressToNode.keys)
      val sourceAddress = shuffledAddresses.head
      val sourceNode = addressToNode(sourceAddress)
      val targetAddress = Random.shuffle(shuffledAddresses.tail).head
      val fee = 100000
      val amount = (Random.nextDouble() * 1e-3 * (balances(sourceAddress) - fee)).toLong

      balances += sourceAddress -> (balances(sourceAddress) - amount)
      balances += targetAddress -> (balances(targetAddress) + amount)

      sourceNode.transfer(sourceAddress, targetAddress, amount, fee)
    }

    Await.result(Future.traverse(1 to 1000)(_ => makeTransfer()), 1.minute)

    eventually(timeout(1.minute), interval(1.second)) {
      val heights = Await.result(Future.traverse(allNodes)(_.height), 30.seconds)
      heights.min should be > 30L
    }

    whenReady(Future.traverse(allNodes)(_.blockAt(30))) { blocks =>
      all(blocks) shouldEqual blocks.head
    }
  }
}
