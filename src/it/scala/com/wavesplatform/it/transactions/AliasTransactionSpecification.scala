package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class AliasTransactionSpecification(override val allNodes: Seq[Node]) extends IntegrationSuiteWithThreeAddresses {
  test("Able to send money to an alias") {
    val alias = "TEST_ALIAS"

    val f = for {
      _ <- assertBalances(firstAddress, 100 waves, 100 waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(aliasTxId))
      _ <- assertBalances(firstAddress, 99 waves, 99 waves)
      transferId <- sender.transfer(firstAddress, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1 waves, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(transferId))
      _ <- assertBalances(firstAddress, 98 waves, 98 waves)
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("Not able to create two aliases to same address") {
    val alias = "TEST_ALIAS2"

    val f = for {
      _ <- assertBalances(firstAddress, 98 waves, 98 waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(aliasTxId))
      _ <- assertBalances(firstAddress, 97 waves, 97 waves)
      _ <- assertBadRequest(sender.createAlias(firstAddress, alias, 1 waves))
    } yield succeed

    Await.result(f, 1 minute)
  }


  test("Not able to create two aliases to other addresses") {
    val alias = "TEST_ALIAS3"

    val f = for {
      _ <- assertBalances(firstAddress, 97 waves, 97 waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(aliasTxId))
      _ <- assertBalances(firstAddress, 96 waves, 96 waves)
      _ <- assertBadRequest(sender.createAlias(secondAddress, alias, 1 waves))
    } yield succeed

    Await.result(f, 1 minute)
  }

  test("alias transaction rollback should works fine") {
    val alias = "TEST_ALIAS4"

    val f = for {
      startHeight <- Future.traverse(allNodes)(_.height).map(_.min)
      _ <- assertBalances(firstAddress, 96 waves, 96 waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(aliasTxId))
      _ <- assertBalances(firstAddress, 95 waves, 95 waves)
      _ <- Future.traverse(allNodes)(_.rollback(startHeight))
      _ <- assertBalances(firstAddress, 96 waves, 96 waves)
      secondAliasTxId <- sender.createAlias(firstAddress, alias, 1 waves).map(_.id)
      _ <- Future.traverse(allNodes)(_.waitForTransaction(secondAliasTxId))
      _ <- assertBalances(firstAddress, 95 waves, 95 waves)
    }  yield succeed

    Await.result(f, 1 minute)
  }
}
