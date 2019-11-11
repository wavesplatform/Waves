package com.wavesplatform.api.common
import org.scalatest.{FreeSpec, Matchers}

import scala.util.Random

class CommonAccountApiSpec extends FreeSpec with Matchers {
  "regex tests" in pendingUntilFixed {
    /*
    val dataKeys = Set(
      "abc",
      "aBcD",
      "ABD",
      "ac",
      "ab1c",
      "1aB1cD",
      "A1BD0",
      "a110b",
      "123",
      "reeee",
      " ab",
      "ab ",
      "a b\n",
      "ab1 \n\t",
      "\n\raB1\t",
      "\n  \r  \t\t",
      "!#$%&'()*+,",
      "!#$%&'()<*=>+,",
      "!",
      "<!@#qwe>",
      "\\",
      "\"\\",
      "qwe!",
      "\b",
      "\u0000",
      "\b\b",
      "\bqweasd\u0000",
      "\u0000qweqwe"
    )

    val testData: Map[String, String] =
      dataKeys.map { k =>
        k -> Random.nextString(16)
      }.toMap

    val regexps = List(
      /*"abc", "bca",*/
      "[a-zA-Z]{1,}",
      "[a-z0-9]{0,4}",
      "[a-zA-Z0-9]{1,4}",
      "[a-z!-/]{2,}",
      "[!-/:-@]{0,}",
      "re*",
      "re.ee",
      "\\w{0,}",
      "\\d{0,}",
      "[\\w\\d]{0,}",
      "\\s{0,}",
      "^1aB1cD$",
      "(a|b)c"
    )
    */


    fail("not implemented")
  }
}
