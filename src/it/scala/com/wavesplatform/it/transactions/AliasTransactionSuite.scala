package com.wavesplatform.it.transactions

import com.wavesplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.Await
import scala.concurrent.duration._

class AliasTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val waitCompletion = 2.minutes
  private val aliasFee = 1.waves
  private val leasingFee = 0.001.waves

  test("Able to send money to an alias") {
    val alias = "test_alias"

    val f = for {
      _ <- assertBalances(firstAddress, 100.waves, 100.waves)
      aliasTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(aliasTxId, 1)

      _ <- assertBalances(firstAddress, 99.waves, 99.waves)
      transferId <- sender.transfer(firstAddress, s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$alias", 1.waves, 1.waves).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(transferId, 1)
      _ <- assertBalances(firstAddress, 98.waves, 98.waves)
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("Not able to create same aliases to same address") {
    val alias = "test_alias2"
    val f = for {
      balance <- accountBalance(firstAddress)
      effectiveBalance <- accountEffectiveBalance(firstAddress)

      aliasTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(aliasTxId, 1)
      newBalance = balance - aliasFee
      newEffectiveBalance = effectiveBalance - aliasFee

      _ <- assertBalances(firstAddress, newBalance, newEffectiveBalance)
      _ <- assertBadRequest(sender.createAlias(firstAddress, alias, aliasFee))
      _ <- assertBalances(firstAddress, newBalance, newEffectiveBalance)
    } yield succeed

    Await.result(f, waitCompletion)
  }


  test("Not able to create aliases to other addresses") {
    val alias = "test_alias3"

    val f = for {
      balance <- accountBalance(firstAddress)
      effectiveBalance <- accountEffectiveBalance(firstAddress)

      aliasTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(aliasTxId, 1)
      _ <- assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, aliasFee), "Tx with such id already present")
      _ <- assertBalances(firstAddress, balance - aliasFee, effectiveBalance - aliasFee)
    } yield succeed

    Await.result(f, waitCompletion)
  }

  test("Able to create several different aliases to same addresses") {
    val firstAlias = "test_alias4"
    val secondAlias = "test_alias5"

    val f = for {

      (balance, effectiveBalance) <- accountBalance(secondAddress).zip(accountEffectiveBalance(secondAddress))

      aliasFirstTxId <- sender.createAlias(secondAddress, firstAlias, aliasFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(aliasFirstTxId, 1)

      newBalance = balance - aliasFee
      newEffectiveBalance = effectiveBalance - aliasFee

      _ <- assertBalances(secondAddress, newBalance, newEffectiveBalance)

      aliasSecondTxId <- sender.createAlias(secondAddress, secondAlias, aliasFee).map(_.id)

      _ <- waitForHeightAraiseAndTxPresent(aliasSecondTxId, 1)
      _ <- assertBalances(secondAddress, newBalance - aliasFee, newEffectiveBalance - aliasFee)

      aliasesList <- sender.aliasByAddress(secondAddress)

    } yield {
      aliasesList should contain allElementsOf Seq(firstAlias, secondAlias)
        .map(s => s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$s")
    }

    Await.result(f, waitCompletion)
  }


  test("Able to get address by alias") {
    val alias = "test_alias_6"
    val f = for {
      balance <- accountBalance(firstAddress)
      aliasFirstTxId <- sender.createAlias(firstAddress, alias, aliasFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(aliasFirstTxId, 1)
      addressByAlias <- sender.addressByAlias(alias).map(_.address)
    } yield {
      addressByAlias should be(firstAddress)
    }

    Await.result(f, waitCompletion)
  }

  val aliases_names =
    Table("aliasName",
      "aaaa",
      "sixteen_chars_al",
      "....",
      "1234567890123456",
      "@.@-@_@")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      val f = for {
        balance <- accountBalance(secondAddress)
        effectiveBalance <- accountEffectiveBalance(secondAddress)
        aliasTxId <- sender.createAlias(secondAddress, alias, aliasFee).map(_.id)
        _ <- waitForHeightAraiseAndTxPresent(aliasTxId, 1)
        _ <- assertBalances(secondAddress, balance - aliasFee, effectiveBalance - aliasFee)

      } yield succeed

      Await.result(f, waitCompletion)
    }

  }

  val invalid_aliases_names =
    Table(("aliasName", "message"),
      ("", "Alias '' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      (null, "failed to parse json message"),
      ("morethan_thirtycharactersinline", "Alias 'morethan_thirtycharactersinline' length should be between 4 and 30"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("multilinetest\ntest", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("UpperCaseAliase", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"))

  forAll(invalid_aliases_names) { (alias: String, message: String) =>
    test(s"Not able to create alias named $alias") {
      val f = for {
        _ <- assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, aliasFee), message)
      } yield succeed

      Await.result(f, waitCompletion)
    }
  }

  test("Able to lease by alias") {
    val thirdAddressAlias = "leasing_alias"
    val buildedThirdAddressAlias = s"alias:${sender.settings.blockchainSettings.addressSchemeCharacter}:$thirdAddressAlias"

    val f = for {
      (((firstAddressBalance, firstAddressEffectiveBalance), thirdAddressBalance), thirdAddressEffectiveBalance) <- accountBalance(firstAddress)
        .zip(accountEffectiveBalance(firstAddress))
        .zip(accountBalance(thirdAddress))
        .zip(accountEffectiveBalance(thirdAddress))

      aliasTxId <- sender.createAlias(thirdAddress, thirdAddressAlias, aliasFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(aliasTxId, 1)

      //lease maximum value, to pass next test
      leasingAmount = firstAddressBalance - leasingFee - 0.5.waves

      leasingTx <- sender.lease(firstAddress, buildedThirdAddressAlias, leasingAmount, leasingFee).map(_.id)
      _ <- waitForHeightAraiseAndTxPresent(leasingTx, 1)

      _ <- assertBalances(firstAddress, firstAddressBalance - leasingFee, firstAddressEffectiveBalance - leasingAmount - leasingFee)
        .zip(assertBalances(thirdAddress, thirdAddressBalance - aliasFee, thirdAddressEffectiveBalance - aliasFee + leasingAmount))
    } yield succeed
    Await.result(f, waitCompletion)
  }

  //previous test should not be commented to run this one
  test("Not able to create aliase when insufficient funds") {
    val alias = "test_alias7"
    val f = for {
      _ <- assertBadRequestAndMessage(sender.createAlias(firstAddress, alias, aliasFee),
        "State check failed. Reason: negative effective balance")
    } yield succeed
    Await.result(f, waitCompletion)
  }
}