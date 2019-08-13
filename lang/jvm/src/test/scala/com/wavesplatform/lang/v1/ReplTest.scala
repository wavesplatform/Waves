package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.{Matchers, PropSpec}

class ReplTest extends PropSpec with ScriptGen with Matchers with NoShrink {
  property("variable memorization") {
    val repl = Repl()
    repl.execute("let a = 1")     shouldBe Right("Unit")
    repl.execute("let b = 2")     shouldBe Right("Unit")
    repl.execute("let c = a + b") shouldBe Right("Unit")
    repl.execute("c")             shouldBe Right("3")
    repl.execute("a + b")         shouldBe Right("3")
  }

  property("context funcs") {
    val repl = Repl()
    repl.execute(""" let s = "aaa|bbb|ccc" """)
    repl.execute(""" s.split("|") """) shouldBe Right("""["aaa", "bbb", "ccc"]""")

    repl.execute(""" let a = blake2b256(base58'') != base58'' """)
    repl.execute(""" a || false """) shouldBe Right("true")
  }

  property("user funcs") {
    val repl = Repl()
    repl.execute(""" func inc(a: Int) = a + 1 """) shouldBe Right("Unit")
    repl.execute(""" inc(5) """) shouldBe Right("6")
  }

  property("syntax errors") {
    val repl = Repl()
    repl.execute(""" let a = {{1} """) shouldBe Left("Compilation failed: expected a value's expression in 9-9")
    repl.execute(""" 1 ++ 2 """)       shouldBe Left("Compilation failed: expected a second operator in 4-4")
  }

  property("logic errors") {
    val repl = Repl()
    repl.execute(""" let a = base64'12345' """) shouldBe Left("Compilation failed: can't parse Base64 string in 17-21")
    repl.execute(""" let b = "abc" + 1 """)     shouldBe Left("Compilation failed: Can't find a function overload '+'(String, Int) in 9-18")
  }

  property("exceptions") {
    val repl = Repl()
    val msg = "error message"
    repl.execute(s""" throw("$msg") """) shouldBe Left(msg)
  }

  property("waves context funcs") {
    val repl = Repl()
    repl.execute(s""" transferTransactionById(base58'fdg') """) shouldBe Left("Blockchain state is unavailable from REPL")
    repl.execute(s""" let a = height """)
    repl.execute(s""" a """)  shouldBe Right("0")
  }

  property("state reset") {
    val repl = Repl()
    repl.execute("let a = 1")     shouldBe Right("Unit")
    repl.execute("let b = a + 2") shouldBe Right("Unit")
    repl.execute("b")             shouldBe Right("3")
    repl.clear()
    repl.execute("a") shouldBe Left("Compilation failed: A definition of 'a' is not found in 0-1")
    repl.execute("b") shouldBe Left("Compilation failed: A definition of 'b' is not found in 0-1")
  }

  property("keep state if input contain both expression and declarations") {
    val repl = Repl()
    repl.execute(
      """
        | func main() = {
        |   3
        | }
        | main()
      """.stripMargin
    ) shouldBe Right("3")
  }

  property("ctx leak") {
    val repl = Repl()
    repl.execute(
      """
         func f() = {
           let a = 3
           a
         }
      """
    )
    repl.execute(
      """
         let b = {
           let a = 3
           a
         }
      """
    )
    repl.execute("f()") shouldBe Right("3")
    repl.execute("b")   shouldBe Right("3")
    repl.execute("a") shouldBe Left("Compilation failed: A definition of 'a' is not found in 0-1")
  }

  property("type info") {
    val repl = Repl()
    repl.info("AttachedPayment") shouldBe "type AttachedPayment { assetId: ByteVector|Unit, amount: Int }"
    repl.info("String")          shouldBe "type String"
  }

  property("func info") {
    val repl = Repl()
    repl.info("getInteger").split("\n") shouldBe Array(
      "func getInteger(addressOrAlias: Address|Alias, key: String): Int|Unit",
      "func getInteger(data: List[DataEntry], key: String): Int|Unit",
      "func getInteger(data: List[DataEntry], index: Int): Int|Unit"
    )
    repl.execute("func my(a: Int) = toString(a)")
    repl.info("my") shouldBe "func my(a: Int): String"
  }

  property("let info") {
    val repl = Repl()
    repl.execute("let a = 5")
    repl.info("a")    shouldBe "let a: Int"
    repl.info("unit") shouldBe "let unit: Unit"
  }

  property("info not found") {
    Repl().info("unexisted") shouldBe "unexisted not found in context"
  }
}
