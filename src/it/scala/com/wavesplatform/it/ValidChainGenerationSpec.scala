package com.wavesplatform.it

import org.scalatest._
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class ValidChainGenerationSpec(docker: Docker) extends FreeSpec with Matchers with ScorexLogging {
  private val nodeConfigs = Random.shuffle(Docker.NodeConfigs.getConfigList("nodes").asScala).take(3)

  "Generate 30 blocks and synchronise" in {
    val allNodes = Await.result(Future.traverse(nodeConfigs)(docker.startNode)
      .map(_.map(n => n.address -> n).toMap), 30.seconds)

    var balances = Await.result(Future.traverse(allNodes.values)(n => n.balance(n.address))
      .map(_.map(b => b.address -> b.balance).toMap), 30.seconds)

    def makeTransfer(): Future[String] = {
      val shuffledAddresses = Random.shuffle(allNodes.keys)
      val sourceAddress = shuffledAddresses.head
      val sourceNode = allNodes(sourceAddress)
      val targetAddress = Random.shuffle(shuffledAddresses.tail).head
      val fee = 100000
      val amount = (Random.nextDouble() * 1e-3 * (balances(sourceAddress) - fee)).toLong

      balances += sourceAddress -> (balances(sourceAddress) - amount)
      balances += targetAddress -> (balances(targetAddress) + amount)

      sourceNode.transfer(sourceAddress, targetAddress, amount, fee)
    }

    log.debug("Generating transactions...")
    Await.result(Future.traverse(1 to 1000)(_ => makeTransfer()), 20.seconds)
    log.debug("Done")
  }
}
