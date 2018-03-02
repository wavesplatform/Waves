package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.util.Random


class AliasTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  private val transferFee = 1.waves
  private val leasingFee = 0.001.waves
  private val transferAmount = 1.waves

  test("Able to send money to an alias") {
    val alias = randomAlias()
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val aliasFee = calcAliasFee(firstAddress, alias)
    notMiner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)

    val aliasFull = fullAliasByAddress(firstAddress, alias)

    val transferId = sender.transfer(firstAddress
        , aliasFull
        , transferAmount
        , transferFee).id

    nodes.waitForHeightAraiseAndTxPresent(transferId)
    notMiner.assertBalances(firstAddress
        , balance1 - transferFee - aliasFee
        , eff1 - transferFee - aliasFee)
  }

  test("Not able to create same aliases to same address") {
    val alias = randomAlias()
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val aliasFee = calcAliasFee(firstAddress, alias)
    notMiner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)

    assertBadRequest(sender.createAlias(firstAddress, alias, transferFee))
    notMiner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)
  }

  test("Not able to create aliases to other addresses") {
    val alias = randomAlias()

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val aliasFee = calcAliasFee(firstAddress, alias)
    assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, transferFee), "already in the state")
    notMiner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)
  }

  test("Able to create several different aliases to same addresses") {
    val firstAlias = randomAlias()
    val secondAlias = randomAlias()

    val (balance1, eff1) = notMiner.accountBalances(secondAddress)

    val aliasFeeFirstAlias = calcAliasFee(secondAddress, firstAlias)
    notMiner.assertBalances(secondAddress, balance1 - aliasFeeFirstAlias, eff1 - aliasFeeFirstAlias)

    val aliasFeeSecondAlias = calcAliasFee(secondAddress, secondAlias)
    notMiner.assertBalances(secondAddress, balance1 - aliasFeeFirstAlias - aliasFeeSecondAlias, eff1 - aliasFeeFirstAlias - aliasFeeSecondAlias)

    val aliasesList = sender.aliasByAddress(secondAddress)
    aliasesList should contain allElementsOf Seq(fullAliasByAddress(secondAddress, firstAlias), fullAliasByAddress(secondAddress, secondAlias))

  }

  val aliases_names =
    Table(s"aliasName${randomAlias()}",
      s"aaaa${randomAlias()}",
      s"....${randomAlias()}",
      s"1234567890.${randomAlias()}",
      s"@.@-@_@${randomAlias()}")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      val (balance1, eff1) = notMiner.accountBalances(secondAddress)
      val aliasFee = calcAliasFee(secondAddress, alias)
      notMiner.assertBalances(secondAddress, balance1 - aliasFee, eff1 - aliasFee)
    }
  }

  val invalid_aliases_names =
    Table(("aliasName", "message"),
      ("", "Alias '' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      (null, "failed to parse json message"),
      ("morethen_thirtycharactersinline", "Alias 'morethen_thirtycharactersinline' length should be between 4 and 30"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("multilnetest\ntest", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("UpperCaseAliase", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"))

  forAll(invalid_aliases_names) { (alias: String, message: String) =>
    test(s"Not able to create alias named $alias") {
      assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, transferFee), message)
    }
  }

  test("Able to lease by alias") {
    val thirdAddressAlias = randomAlias()

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance3, eff3) = notMiner.accountBalances(thirdAddress)

    val aliasFee = calcAliasFee(thirdAddress, thirdAddressAlias)
    val aliasFull = fullAliasByAddress(thirdAddress, thirdAddressAlias)
    //lease maximum value, to pass next thirdAddress
    val leasingAmount = balance1 - leasingFee - 0.5.waves

    val leasingTx = sender.lease(firstAddress, aliasFull, leasingAmount, leasingFee).id
    nodes.waitForHeightAraiseAndTxPresent(leasingTx)

    notMiner.assertBalances(firstAddress, balance1 - leasingFee, eff1 - leasingAmount - leasingFee)
    notMiner.assertBalances(thirdAddress, balance3 - aliasFee, eff3 - aliasFee + leasingAmount)

  }

   //previous test should not be commented to run this one
  test("Not able to create aliase when insufficient funds") {
    val alias = randomAlias()
    assertBadRequestAndMessage(sender.createAlias(firstAddress, alias, transferFee),
        "State check failed. Reason: negative effective balance")
  }

  private def calcAliasFee(address: String, alias: String): Long = {
    if (!sender.aliasByAddress(address).exists(_.endsWith(alias))){
      val aliasId = sender.createAlias(address, alias, transferFee).id
      nodes.waitForHeightAraiseAndTxPresent(aliasId)
      transferFee
    } else 0
  }

  private def fullAliasByAddress(address: String, alias: String): String = {
    sender.aliasByAddress(address).find(_.endsWith(alias)).get
  }
  
  private def randomAlias(): String = {
    s"testalias.${Random.alphanumeric take 9 mkString}".toLowerCase
  }

}
