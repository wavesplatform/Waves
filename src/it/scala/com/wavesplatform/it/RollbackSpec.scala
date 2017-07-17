package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.collection.mutable
import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class RollbackSpec(override val allNodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with TransferSending {
  "Apply 200 transfer transactions twice with rollback " in {
    val node = allNodes.head
    result(for {
      b <- traverse(allNodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      startHeight <- Future.traverse(allNodes)(_.height).map(_.min)
      requests = generateRequests(200, b)
      _ <- processRequests(requests)
      afterFirstTryHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 7).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      _ <- traverse(allNodes)(_.rollback(startHeight))
      afterSecondTryHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 7).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
    } yield {
      afterFirstTryHash shouldBe afterSecondTryHash
    }, 5.minutes)
  }
}
