package com.wavesplatform.it

import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.Await.result
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse


class NetworkSeparationTest(allNodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers {
  "nodes should resolve forks correctly after network separation" in {
    val targetBlocks = result(for {
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- {
        traverse(allNodes)(_.waitForHeight(height + 10)).map(_ =>
          allNodes.foreach(_.disconnectFromNetwork())
        )
      }
      _ <- {
        traverse(allNodes)(_.waitForHeight(height + 20)).map(_ =>
          allNodes.foreach(_.connectToNetwork()))
      }
      _ <- traverse(allNodes)(_.waitForHeight(height + 50))
      blocks <- traverse(allNodes)(_.blockAt(height + 50))
    } yield blocks, 5.minutes)

    all(targetBlocks) shouldEqual targetBlocks.head
  }
}
