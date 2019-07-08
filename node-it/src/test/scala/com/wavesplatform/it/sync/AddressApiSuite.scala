package com.wavesplatform.it.sync

import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.it.api.SyncHttpApi._

import scala.util.Random

class AddressApiSuite extends BaseTransactionSuite {

  test("filter accounts data by regexp") {
    val dataKeys         = List("1aB1cD!@#$", "\"\\", "\0qweqwe", "\t\r\n")
    val regexps          = List("1aB1cD!@#$", "[a-zA-Z0-9!-/:-@\\\\]{0,15}", "\\s{0,}")
    val invalidRegexps   = List("[a-z", "[a-z]{0", "[a-z]{,5}")
    val data             = dataKeys.map(str => StringDataEntry(str, Random.nextString(16)))
    val transferFee      = calcDataFee(data)
    val txId             = sender.putData(firstAddress, data, transferFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    for (regexp <- regexps) {
      val matchedDataKeys = sender.getData(firstAddress, regexp).sortBy(_.key)

      val regexpPattern = regexp.r.pattern
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
