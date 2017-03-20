package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}
import scorex.api.http.alias.CreateAliasRequest

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

class AliasTransactionSpec(allNodes: Seq[Node]) extends FreeSpec with Matchers {
  "Able to send money to an alias" in {
    val Seq(creator, sender) = Random.shuffle(allNodes).take(2)

    val transferResult = for {
      _ <- creator.post("/alias/create", CreateAliasRequest(creator.address, "TEST-ALIAS", 100000))
      b <- creator.waitForNextBlock
      _ <- sender.waitForHeight(b.height)
      t <- sender.transfer(sender.address, "alias:T:TEST_ALIAS", 1000000, 100000)
    } yield t

    println(Await.result(transferResult, 10.seconds))
  }

  "Able to issue an alias and send money to an alias" in {

  }

  "unable to send to non-issued alias" in {

  }

  "unable to lease to non-issued alias" in {

  }

  "unable to create the same alias in the next block" in {

  }

  "able to recreate alias after rollback" in pending
}
