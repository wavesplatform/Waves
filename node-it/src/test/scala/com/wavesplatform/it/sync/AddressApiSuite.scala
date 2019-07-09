package com.wavesplatform.it.sync

import java.net.URLDecoder

import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.it.api.SyncHttpApi._

import scala.util.Random

class AddressApiSuite extends BaseTransactionSuite {

  test("filter accounts data by regexp") {
    val dataKeys         = List("1aB1cD!@#$", "\"\\", "\u0000qweqwe", "\t\r\n", "reeeee", "rerere", "rerrre", "rre", "eeeee")
    val regexps          = List("1aB1cD!%40%23%24", "%5Ba-zA-Z0-9!-%2F%3A-%40%5C%5C%5C%5C%5D%7B0%2C15%7D", "%5Cs%7B0%2C%7D", "re%2B", "re*re", "r%3Feeeee", "%5B%5Ct%5Cr%5Cn%5D")
    val invalidRegexps   = List("%5Ba-z", "%5Ba-z%5D%7B0", "%5Ba-z%5D%7B%2C5%7D")
    val data             = dataKeys.map(str => StringDataEntry(str, Random.nextString(16)))
    val transferFee      = calcDataFee(data)
    val txId             = sender.putData(firstAddress, data, transferFee).id
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
        case err: Throwable => if (!err.getMessage.contains("Cannot compile regex")) {
          throw err
        }
      }
    }
  }
}
