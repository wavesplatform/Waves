package com.wavesplatform.it.sync

import java.net.URLDecoder
import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.{CustomValidationError, TooBigArrayAllocation}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.{NTPTime, NodeConfigs}
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.TxVersion
import play.api.libs.json.*

import scala.util.Random

class AddressApiSuite extends BaseTransactionSuite with NTPTime {
  test("balance at height") {
    val address = sender.createKeyPair().toAddress.toString
    sender.transfer(sender.keyPair, address, 1, waitForTx = true)
    nodes.waitForHeightArise()
    sender.transfer(sender.keyPair, address, 1, waitForTx = true)
    nodes.waitForHeightArise()
    sender.transfer(sender.keyPair, address, 1, waitForTx = true)
    nodes.waitForHeightArise()

    val Seq(_, h2, _)     = sender.debugBalanceHistory(address): @unchecked
    val Seq((_, balance)) = sender.accountsBalances(Some(h2.height), Seq(address)): @unchecked
    balance shouldBe 2
  }

  test("filter accounts data by regexp") {
    val dataKeys = List("1aB1cD!@#$", "\"\\", "\u0000qweqwe", "\t\r\n", "reeeee", "rerere", "rerrre", "rre", "eeeee")
    val regexps = List(
      "1aB1cD!%40%23%24",
      "%5Ba-zA-Z0-9!-%2F%3A-%40%5C%5C%5C%5C%5D%7B0%2C15%7D",
      "%5Cs%7B0%2C%7D",
      "re%2B",
      "re*re",
      "r%3Feeeee",
      "%5B%5Ct%5Cr%5Cn%5D"
    )
    val invalidRegexps = List("%5Ba-z", "%5Ba-z%5D%7B0", "%5Ba-z%5D%7B%2C5%7D")
    val data           = dataKeys.map(str => StringDataEntry(str, Random.nextString(16)))
    val dataFee        = calcDataFee(data, TxVersion.V1)
    val txId           = sender.putData(firstKeyPair, data, dataFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    for (regexp <- regexps) {
      val matchedDataKeys = sender.getData(firstAddress, regexp).sortBy(_.key)

      val regexpPattern = URLDecoder.decode(regexp, "UTF-8").r.pattern
      withClue(s"regexp: $regexp\n") {
        data.filter(d => regexpPattern.matcher(d.key).matches).sortBy(_.key) shouldEqual matchedDataKeys
      }
    }

    for (invalidRegexp <- invalidRegexps) {
      assertBadRequestAndMessage(
        sender.getData(firstAddress, invalidRegexp),
        "Cannot compile regex"
      )
    }
  }

  test("balances for waves should be correct") {
    assertBalances(None)
  }

  test("balances for issued asset should be correct") {
    val asset = miner.issue(miner.keyPair, "Test", "Test", 10000, 0, waitForTx = true).id
    assertBalances(Some(asset))
  }

  test("limit violation requests should be handled correctly") {
    val limit     = miner.config.getInt("waves.rest-api.transactions-by-address-limit")
    val addresses = List.fill(limit + 1)(firstAddress)
    assertApiError(
      miner.get(s"/addresses/balance?${addresses.map(a => s"address=$a").mkString("&")}"),
      TooBigArrayAllocation
    )
    assertApiError(
      miner.accountsBalances(None, addresses),
      TooBigArrayAllocation
    )
  }

  test("requests to the illegal height should be handled correctly ") {
    val height = miner.height + 100
    assertApiError(
      miner.get(s"/addresses/balance?height=$height&address=$firstKeyPair"),
      CustomValidationError(s"Illegal height: $height")
    )
    assertApiError(
      miner.accountsBalances(Some(height), Seq("firstAddress")),
      CustomValidationError(s"Illegal height: $height")
    )

    assertApiError(
      miner.get(s"/addresses/balance?height=-1&address=$firstKeyPair"),
      CustomValidationError("Illegal height: -1")
    )
    assertApiError(
      miner.accountsBalances(Some(-1), Seq("firstAddress")),
      CustomValidationError("Illegal height: -1")
    )
  }

  private def assertBalances(asset: Option[String]): Unit = {
    val addressesAndBalances = (1 to 5).map(i => (miner.createKeyPair().toAddress.toString, (i * 100).toLong)).toList

    val firstAddresses   = addressesAndBalances.slice(0, 2)
    val secondAddresses  = addressesAndBalances.slice(2, 5)
    val illegalAddresses = List.fill(3)(Random.nextString(10))

    val heightBefore  = transferAndReturnHeights(firstAddresses, asset).min - 1
    val heightBetween = nodes.waitForHeightArise()
    nodes.waitForHeightArise() // prevents next transfers from accepting on the heightBetween
    val heightAfter = transferAndReturnHeights(secondAddresses, asset).max

    nodes.waitForHeightArise()

    val requestedAddresses = addressesAndBalances.map(_._1) ++ illegalAddresses :+ firstAddresses.head._1 :+ secondAddresses.head._1

    // balances at the height before all transfers
    checkBalances(addressesAndBalances.map { case (a, _) => (a, 0L) }, requestedAddresses, Some(heightBefore), asset)
    // balances at the height after the 2nd transfer
    checkBalances(firstAddresses ++ secondAddresses.map { case (a, _) => (a, 0L) }, requestedAddresses, Some(heightBetween), asset)
    // balances at the height after all transfers
    checkBalances(addressesAndBalances, requestedAddresses, Some(heightAfter), asset)
    // balances at the current height
    checkBalances(addressesAndBalances, requestedAddresses, None, asset)
  }

  private def transferAndReturnHeights(addresses: List[(String, Long)], asset: Option[String]): List[Int] = {
    val ids = addresses.map { case (address, a) => miner.transfer(miner.keyPair, address, a, minFee, asset).id }
    ids.map(id => miner.waitForTransaction(id).height)
  }

  private def checkBalances(expected: List[(String, Long)], addresses: List[String], height: Option[Int], assetId: Option[String]): Unit = {
    val getResponse = miner.get(
      s"/addresses/balance?${height.fold("")(h => s"height=$h&")}${assetId.fold("")(a => s"asset=$a&")}${addresses.map(a => s"address=$a").mkString("&")}"
    )

    val getResult = Json.parse(getResponse.getResponseBody).as[List[JsObject]].map(r => ((r \ "id").as[String], (r \ "balance").as[Long]))

    getResult should contain theSameElementsAs expected

    val postResult = miner.accountsBalances(height, addresses, assetId)

    postResult should contain theSameElementsAs expected
  }

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.rest-api.transactions-by-address-limit = 20"))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}
