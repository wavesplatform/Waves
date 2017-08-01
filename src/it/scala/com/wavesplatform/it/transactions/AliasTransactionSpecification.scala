package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import com.wavesplatform.it.{IntegrationSuiteWithThreeAddresses, Node}
import com.wavesplatform.state2._
import org.scalatest.prop.TableDrivenPropertyChecks
import scorex.account.Alias

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._


class AliasTransactionSpecification(override val allNodes: Seq[Node], override val notMiner: Node)
  extends IntegrationSuiteWithThreeAddresses with TableDrivenPropertyChecks {

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


  ignore("Not able to create aliases to other addresses") {
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
      _ <- assertBalances(firstAddress, balance - aliasFee, effectiveBalance - aliasFee)
    } yield succeed

    Await.result(f, 1.minute)
  }

  ignore("Able to create several different aliases to same addresses") {
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
      aliasesList should contain allElementsOf Seq(first_alias, second_alias).map(s => Alias.buildAlias('I', s).explicitGet().stringRepr)
    }

    Await.result(f, 1.minute)
  }


  //  def waitForHeightAraise(transactionId: String, heightIncreaseOn: Integer)  {
  //    var f = for {
  //      height <- traverse(allNodes)(_.height).map(_.max)
  //      _ <- traverse(allNodes)(_.waitForHeight(height + heightIncreaseOn))
  //      _ <- traverse(allNodes)(_.waitForTransaction(transactionId))
  //    }
  //      Await.result(f, 1.minute)
  //  }

  ignore("Able to get address by alias") {
    val alias = "test_alias_6"
    var f = for {
      balance <- getAccountBalance(firstAddress)
      aliasFirstTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)
      height <- traverse(allNodes)(_.height).map(_.max)
      _ <- traverse(allNodes)(_.waitForHeight(height + 1))
      _ <- traverse(allNodes)(_.waitForTransaction(aliasFirstTxId))
      //      obj <- waitForHeightAraise(aliasFirstTxId, 1)
      addressByAlias <- sender.addressByAlias(alias).map(_.address)
    } yield {
      addressByAlias should be(s"address:$firstAddress")
    }

    Await.result(f, 1.minute)
  }

  val aliases_names =
    Table("alias_name",
      "aaaa",
      "sixteen_chars_al",
      "....",
      "1234567890123456",
      "@.@-@_@"
    ,"UpperCaseAliase")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      val f = for {
        balance <- getAccountBalance(secondAddress)
        effectiveBalance <- getAccountEffectiveBalance(secondAddress)
        aliasTxId <- sender.createAlias(secondAddress, alias, aliasFee).map(_.id)

        height <- traverse(allNodes)(_.height).map(_.max)
        _ <- traverse(allNodes)(_.waitForHeight(height + 1))
        _ <- traverse(allNodes)(_.waitForTransaction(aliasTxId))
        _ <- assertBalances(secondAddress, balance - aliasFee, effectiveBalance - aliasFee)

      } yield succeed

      Await.result(f, 1.minute)
    }

  }

  val invalid_aliases_names =
    Table(("alias_name", "message"),
      ("", "Alias 'abc' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      (null, "Alias 'abc' length should be between 4 and 30"),
      ("morethen_thirtycharactersinline", ""),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", ""),
      ("multilnetest\ntest", "Alias cannot be multiline"),
      ("UpperCaseAliase", ""))

  forAll(invalid_aliases_names) { (alias: String, message: String) =>
    ignore(s"Not able to create alias named $alias") {
      val f = for {
        _ <- assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, aliasFee), message)
      } yield succeed

      Await.result(f, 1.minute)
    }


  }
}