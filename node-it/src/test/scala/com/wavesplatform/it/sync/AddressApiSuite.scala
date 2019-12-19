package com.wavesplatform.it.sync

import java.net.URLDecoder

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.TooBigArrayAllocation
import com.wavesplatform.it.{NTPTime, NodeConfigs}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.StringDataEntry
import org.asynchttpclient.Response
import play.api.libs.json._

import scala.util.Random

class AddressApiSuite extends BaseTransactionSuite with NTPTime {

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
    val transferFee    = calcDataFee(data)
    val txId           = sender.putData(firstAddress, data, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    for (regexp <- regexps) {
      val matchedDataKeys = sender.getData(firstAddress, regexp).sortBy(_.key)

      val regexpPattern = URLDecoder.decode(regexp, "UTF-8").r.pattern
      withClue(s"regexp: $regexp\n") {
        data.filter(d => regexpPattern.matcher(d.key).matches).sortBy(_.key) shouldEqual matchedDataKeys
      }
    }

    for (invalidRegexp <- invalidRegexps) {
      try {
        sender.getData(firstAddress, invalidRegexp)
        fail("RegexCompiler didn't throw expected error")
      } catch {
        case err: Throwable =>
          if (!err.getMessage.contains("Cannot compile regex")) {
            throw err
          }
      }
    }
  }

  test("balances for waves should be correct") {
    assertBalances(None)
  }

  test("balances for issued asset should be correct") {
    val asset = miner.issue(miner.address, "Test", "Test", 10000, 0, waitForTx = true).id
    assertBalances(Some(asset))
  }

  test("limit violation requests should be handled") {
    val limit     = miner.config.getInt("waves.rest-api.transactions-by-address-limit")
    val address   = miner.createAddress()
    val addresses = List.fill(limit + 1)(address)
    assertApiError(
      miner.get(s"/addresses/balance?${addresses.map(a => s"address=$a").mkString("&")}"),
      TooBigArrayAllocation
    )
    assertApiError(
      miner.postJson(
        "/addresses/balance",
        Json.obj("ids" -> addresses)
      ),
      TooBigArrayAllocation
    )
  }

  private def assertBalances(asset: Option[String]): Unit = {
    val addresses = (1 to 5).map(i => (miner.createAddress(), (i * 100).toLong)).toList

    val firstAddresses  = addresses.slice(0, 2)
    val secondAddresses = addresses.slice(2, 5)

    val heightBefore  = transferAndReturnHeights(firstAddresses, asset).min - 1
    val heightBetween = nodes.waitForHeightArise()
    nodes.waitForHeightArise() // prevents next transfers from accepting on the heightBetween
    val heightAfter = transferAndReturnHeights(secondAddresses, asset).max

    nodes.waitForHeightArise()

    checkBalances(List(), addresses.map(_._1), Some(heightBefore), asset)          // balances at the height before all transfers
    checkBalances(firstAddresses, addresses.map(_._1), Some(heightBetween), asset) // balances at the height after the 2nd transfer
    checkBalances(addresses, addresses.map(_._1), Some(heightAfter), asset)        // balances at the height after all transfers
    checkBalances(addresses, addresses.map(_._1), None, asset)                     // balances at the current height
  }

  private def transferAndReturnHeights(addresses: List[(String, Long)], asset: Option[String]): List[Int] = {
    val ids = addresses.map { case (address, a) => miner.transfer(miner.address, address, a, minFee, asset).id }
    ids.map(id => miner.waitForTransaction(id).height)
  }

  private def checkBalances(expected: List[(String, Long)], addresses: List[String], height: Option[Int], assetId: Option[String]): Unit = {
    val getResult = miner.get(
      s"/addresses/balance?${height.fold("")(h => s"height=$h&")}${assetId.fold("")(a => s"asset=$a&")}${addresses.map(a => s"address=$a").mkString("&")}"
    )

    asResult(getResult) should contain theSameElementsAs expected

    val postResult = miner.postJson(
      "/addresses/balance",
      Json.obj("ids" -> addresses) ++
        height.fold(Json.obj())(h => Json.obj("height" -> h)) ++
        assetId.fold(Json.obj())(a => Json.obj("asset" -> a))
    )

    asResult(postResult) should contain theSameElementsAs expected
  }

  private def asResult(json: Response): List[(String, Long)] =
    Json.parse(json.getResponseBody).as[List[JsObject]].map(r => ((r \ "id").as[String], (r \ "balance").as[Long]))

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("waves.rest-api.transactions-by-address-limit = 10"))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()
}
