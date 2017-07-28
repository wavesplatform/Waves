package com.wavesplatform.it.transactions

import com.wavesplatform.it.api.NodeApi
import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import scorex.account.Alias
import com.wavesplatform.state2._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class AliasTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses  {

  val aliasFee = 1.waves;

  ignore("Able to send money to an alias") {
    val alias = "test_alias"

    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, 1.waves).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      _ <- assertBalances(firstAddress, 99.waves, 99.waves)
      transferId <- sender.transfer(firstAddress, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1.waves, 1.waves).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(transferId))

      _ <- assertBalances(firstAddress, 98.waves, 98.waves)
    } yield succeed

    Await.result(f, 1.minute)
  }

  ignore("Not able to create same aliases to same address") {
    val alias = "test_alias2"

    val f = for {
      balance <- getAccountBalance(firstAddress)
      effectiveBalance <- getAccountEffectiveBalance(firstAddress)

      aliasTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))

      newBalance = balance - aliasFee
      newEffectiveBalance = effectiveBalance - aliasFee

      _ <- assertBalances(firstAddress, newBalance, newEffectiveBalance)
      _ <- assertBadRequest(sender.createAlias(firstAddress, alias, aliasFee))
      _ <- assertBalances(firstAddress, newBalance, newEffectiveBalance)
    } yield succeed

    Await.result(f, 1.minute)
  }


  test("Not able to create aliases to other addresses") {
    val alias = "test_alias3"

    val f = for {
      balance <- getAccountBalance(firstAddress)
      effectiveBalance <- getAccountEffectiveBalance(firstAddress)

      aliasTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))
      _ <- assertBadRequest(sender.createAlias(secondAddress, alias, aliasFee))
      //todo add request with error deserialization
      _ <- assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, aliasFee), "Tx with such id aready present")
      //the balance should not be changed
      _ <- assertBalances(firstAddress, balance - aliasFee, effectiveBalance - aliasFee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  test("Able to create several different aliases to same addresses") {
    val first_alias = "test_alias4"
    val second_alias = "test_alias5"

    val f = for {

      balance <- getAccountBalance(secondAddress)
      effectiveBalance <- getAccountEffectiveBalance(secondAddress)

      aliasFirstTxId <- sender.createAlias(secondAddress, first_alias, aliasFee).map(_.id)

      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasFirstTxId))

      newBalance = balance - aliasFee
      newEffectiveBalance = effectiveBalance - aliasFee

      _ <- assertBalances(secondAddress, newBalance, newEffectiveBalance)

      aliasSecondTxId <- sender.createAlias(secondAddress, second_alias, aliasFee).map(_.id)


      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasFirstTxId))

      _ <- assertBalances(secondAddress, newBalance - aliasFee, newEffectiveBalance - aliasFee)

      aliasesList <- sender.aliasByAddress(secondAddress)

    } yield {
      aliasesList should contain allElementsOf Seq(first_alias, second_alias).map(s => Alias.buildAlias('I',s).explicitGet().stringRepr)
    }

    Await.result(f, 1.minute)
  }


  test("just print balance") {

    println(getAccountBalance(firstAddress))
    println(getAccountEffectiveBalance(firstAddress))
    println(getAccountBalance(secondAddress))
    println(getAccountEffectiveBalance(secondAddress))
    println(getAccountBalance(thirdAddress))
    println(getAccountEffectiveBalance(thirdAddress))
    //    val alias = "TEST_ALIAS3"
    //
    //    val aliasTxId: NodeApi.Transaction = Await.result(sender.createAlias(firstAddress, alias, 1.waves), 5.second)
    //    val eventualInts: Seq[Future[Int]] = allNodes.map(_.height)
  }


}
