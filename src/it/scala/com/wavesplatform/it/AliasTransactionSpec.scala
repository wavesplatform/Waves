package com.wavesplatform.it

import org.scalatest.{FreeSpec, Matchers}
import scorex.transaction.TransactionParser.TransactionType

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random

class AliasTransactionSpec(allNodes: Seq[Node]) extends FreeSpec with Matchers {
  "Able to send money to an alias" in {
    val alias = "TEST_ALIAS"
    val Seq(creator, sender) = Random.shuffle(allNodes).take(2)

    val createAliasFee = creator.fee(TransactionType.CreateAliasTransaction)
    val transferFee = creator.fee(TransactionType.TransferTransaction)

    val transferResult = for {
      fb <- creator.lastBlock
      t <- creator.createAlias(creator.address, alias, createAliasFee)
      b <- creator.waitForTransaction(t.id)
      _ <- sender.waitForTransaction(t.id)
      t <- sender.transfer(sender.address, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1000000, transferFee)
    } yield t

    Await.result(transferResult, 1.minute)
  }

  "Able to issue an alias and send money to an alias" in {
    pending
  }

  "unable to send to non-issued alias" in {
    pending
  }

  "unable to lease to non-issued alias" in {
    pending
  }

  "unable to create the same alias in the next block" in {
    pending
  }

  "able to recreate alias after rollback" in pending
}
