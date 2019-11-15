package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.Random

class AliasTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {
  test("Able to send money to an alias") {
    val alias            = randomAlias()
    val (balance1, eff1) = miner.accountBalances(firstAddress)

    val aliasFee = createAlias(firstAddress, alias)
    miner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)

    val aliasFull = fullAliasByAddress(firstAddress, alias)

    val transferId = sender.transfer(firstAddress, aliasFull, transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(transferId)
    miner.assertBalances(firstAddress, balance1 - minFee - aliasFee, eff1 - minFee - aliasFee)
  }

  test("Not able to create same aliases to same address") {
    val alias            = randomAlias()
    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val aliasFee         = createAlias(firstAddress, alias)
    miner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)

    assertApiErrorRaised(sender.createAlias(firstAddress, alias, minFee))
    miner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)
  }

  test("Not able to create aliases to other addresses") {
    val alias = randomAlias()

    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val aliasFee         = createAlias(firstAddress, alias)
    assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, minFee), "Alias already claimed")
    miner.assertBalances(firstAddress, balance1 - aliasFee, eff1 - aliasFee)
  }

  test("Able to create several different aliases to same addresses") {
    val firstAlias  = randomAlias()
    val secondAlias = randomAlias()

    val (balance1, eff1) = miner.accountBalances(secondAddress)

    val aliasFeeFirstAlias = createAlias(secondAddress, firstAlias)
    miner.assertBalances(secondAddress, balance1 - aliasFeeFirstAlias, eff1 - aliasFeeFirstAlias)

    val aliasFeeSecondAlias = createAlias(secondAddress, secondAlias)
    miner.assertBalances(secondAddress, balance1 - aliasFeeFirstAlias - aliasFeeSecondAlias, eff1 - aliasFeeFirstAlias - aliasFeeSecondAlias)

    val aliasesList = sender.aliasByAddress(secondAddress)
    aliasesList should contain allElementsOf Seq(fullAliasByAddress(secondAddress, firstAlias), fullAliasByAddress(secondAddress, secondAlias))

  }

  val aliases_names =
    Table(s"aliasName${randomAlias()}", s"aaaa${randomAlias()}", s"....${randomAlias()}", s"1234567890.${randomAlias()}", s"@.@-@_@${randomAlias()}")

  aliases_names.foreach { alias =>
    test(s"create alias named $alias") {
      val (balance1, eff1) = miner.accountBalances(secondAddress)
      val aliasFee         = createAlias(secondAddress, alias)
      miner.assertBalances(secondAddress, balance1 - aliasFee, eff1 - aliasFee)
    }
  }

  val invalid_aliases_names =
    Table(
      ("aliasName", "message"),
      ("", "Alias '' length should be between 4 and 30"),
      ("abc", "Alias 'abc' length should be between 4 and 30"),
      (null, "failed to parse json message"),
      ("morethen_thirtycharactersinline", "Alias 'morethen_thirtycharactersinline' length should be between 4 and 30"),
      ("~!|#$%^&*()_+=\";:/?><|\\][{}", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("multilnetest\ntest", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz"),
      ("UpperCaseAliase", "Alias should contain only following characters: -.0123456789@_abcdefghijklmnopqrstuvwxyz")
    )

  forAll(invalid_aliases_names) { (alias: String, message: String) =>
    test(s"Not able to create alias named $alias") {
      assertBadRequestAndMessage(sender.createAlias(secondAddress, alias, minFee), message)
    }
  }

  test("Able to lease by alias") {
    val thirdAddressAlias = randomAlias()

    val (balance1, eff1) = miner.accountBalances(firstAddress)
    val (balance3, eff3) = miner.accountBalances(thirdAddress)

    val aliasFee  = createAlias(thirdAddress, thirdAddressAlias)
    val aliasFull = fullAliasByAddress(thirdAddress, thirdAddressAlias)
    //lease maximum value, to pass next thirdAddress
    val leasingAmount = balance1 - minFee - 0.5.waves

    val leasingTx = sender.lease(firstAddress, aliasFull, leasingAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(leasingTx)

    miner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
    miner.assertBalances(thirdAddress, balance3 - aliasFee, eff3 - aliasFee + leasingAmount)

  }

  //previous test should not be commented to run this one
  test("Not able to create alias when insufficient funds") {
    val balance = miner.accountBalances(firstAddress)._1
    val alias   = randomAlias()
    assertBadRequestAndMessage(sender.createAlias(firstAddress, alias, balance + minFee), "State check failed. Reason: negative waves balance")
  }

  private def createAlias(address: String, alias: String): Long = {
    if (!sender.aliasByAddress(address).exists(_.endsWith(alias))) {
      val aliasId = sender.createAlias(address, alias, minFee).id
      nodes.waitForHeightAriseAndTxPresent(aliasId)
      minFee
    } else 0
  }

  private def fullAliasByAddress(address: String, alias: String): String = {
    sender.aliasByAddress(address).find(_.endsWith(alias)).get
  }

  private def randomAlias(): String = {
    s"testalias.${Random.alphanumeric.take(9).mkString}".toLowerCase
  }

}
