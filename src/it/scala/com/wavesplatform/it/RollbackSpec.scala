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
  "Apply 100 transfer transactions twice with rollback " in {
    result(for {
      b <- traverse(allNodes)(balanceForNode).map(mutable.AnyRefMap[String, Long](_: _*))
      startHeight <- Future.traverse(allNodes)(_.height).map(_.max)
      startHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 5).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      requests = generateRequests(500, b)
      _ <- processRequests(requests)
      afterFirstTryHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 15).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      _ <- traverse(allNodes)(_.rollback(startHeight))
      secondStartHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 5).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
      afterSecondTryHash <- traverse(allNodes)(_.waitForDebugInfoAt(startHeight + 15).map(_.stateHash)).map(infos => {
        all(infos) shouldEqual infos.head
        infos.head
      })
    } yield {
      startHash shouldBe secondStartHash
      afterFirstTryHash shouldBe afterSecondTryHash
    }, 5.minutes)
  }
}
