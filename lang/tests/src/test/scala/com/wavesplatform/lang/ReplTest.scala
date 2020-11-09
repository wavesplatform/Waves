package com.wavesplatform.lang

import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class ReplTest extends PropSpec with ScriptGen with Matchers with NoShrink {
  def await[A](f: Future[A]): A = Await.result(f, 2 seconds)

  property("variable memorization") {
    val repl = Repl()
    await(repl.execute("let a = 1"))     shouldBe Right("defined let a: Int")
    await(repl.execute("let b = 2"))     shouldBe Right("defined let b: Int")
    await(repl.execute("let c = a + b")) shouldBe Right("defined let c: Int")
    await(repl.execute("c"))             shouldBe Right("res1: Int = 3")
    await(repl.execute("a + b"))         shouldBe Right("res2: Int = 3")
    await(repl.execute("res1 + res2"))   shouldBe Right("res3: Int = 6")
  }

  property("context funcs") {
    val repl = Repl()
    await(repl.execute(""" let s = "aaa|bbb|ccc" """))
    await(repl.execute(""" s.split("|") """)) shouldBe Right("""res1: List[String] = ["aaa", "bbb", "ccc"]""")

    await(repl.execute(""" let a = blake2b256(base58'') != base58'' """))
    await(repl.execute(""" a || false """)) shouldBe Right("res2: Boolean = true")
  }

  property("user funcs") {
    val repl = Repl()
    await(repl.execute(""" func inc(a: Int) = a + 1 """)) shouldBe Right("defined func inc(a: Int): Int")
    await(repl.execute(""" inc(5) """)) shouldBe Right("res1: Int = 6")
  }

  property("syntax errors") {
    val repl = Repl()
    await(repl.execute(""" let a = {{1} """)) shouldBe Left("Compilation failed: [expected a value's expression in 9-9]")
    await(repl.execute(""" 1 %% 2 """))       shouldBe Left("Compilation failed: [expected a second operator in 4-4]")
  }

  property("logic errors") {
    val repl = Repl()
    await(repl.execute(""" let a = base64'12345' """)) shouldBe Left("Compilation failed: can't parse Base64 string in 17-21")
    await(repl.execute(""" let b = "abc" + 1 """))     shouldBe Left("Compilation failed: [Can't find a function overload '+'(String, Int) in 9-18]")
  }

  property("exceptions") {
    val repl = Repl()
    val msg = "error message"
    await(repl.execute(s""" throw("$msg") """)) shouldBe Left(msg)
    await(repl.execute(s""" throw() """)) shouldBe Left("Explicit script termination")
    await(repl.execute(s""" throw("") """)) shouldBe Left("Evaluation error")
  }

  property("waves context funcs absent") {
    val repl = Repl()

    await(repl.execute(s""" transferTransactionById(base58'fdg') """)) should produce("Blockchain state is unavailable from REPL")

    await(repl.execute(s""" let a = 1 """))
    await(repl.execute(s""" a """))  shouldBe Right("res1: Int = 1")
  }

  property("state reset") {
    val repl = Repl()
    await(repl.execute("let a = 1"))     shouldBe Right("defined let a: Int")
    await(repl.execute("let b = a + 2")) shouldBe Right("defined let b: Int")
    await(repl.execute("b"))             shouldBe Right("res1: Int = 3")
    repl.clear()
    await(repl.execute("a")) shouldBe Left("Compilation failed: [A definition of 'a' is not found in 0-1]")
    await(repl.execute("b")) shouldBe Left("Compilation failed: [A definition of 'b' is not found in 0-1]")
  }

  property("keep state if input contain both expression and declarations") {
    val repl = Repl()
    await(repl.execute(
      """
        | func my1() = 3
        | let a = 2
        | let b = 7
        | func my2(a: String) = a + a
        | my1()
      """.stripMargin
    )) shouldBe Right(
      """
        |defined func my1(): Int
        |defined func my2(a: String): String
        |defined let a: Int
        |defined let b: Int
        |res1: Int = 3
      """.stripMargin.trim
    )
  }

  property("ctx leak") {
    val repl = Repl()
    await(repl.execute(
      """
         func f() = {
           let a = 3
           a
         }
      """
    ))
    await(repl.execute(
      """
         let b = {
           let a = 3
           a
         }
      """
    ))
    await(repl.execute("f()")) shouldBe Right("res1: Int = 3")
    await(repl.execute("b"))   shouldBe Right("res2: Int = 3")
    await(repl.execute("a"))   shouldBe Left("Compilation failed: [A definition of 'a' is not found in 0-1]")
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
      "func getInteger(data: List[BinaryEntry|BooleanEntry|DeleteEntry|IntegerEntry|StringEntry], key: String): Int|Unit",
      "func getInteger(data: List[BinaryEntry|BooleanEntry|DeleteEntry|IntegerEntry|StringEntry], index: Int): Int|Unit"
    )
    await(repl.execute("func my(a: Int) = toString(a)"))
    repl.info("my") shouldBe "func my(a: Int): String"
  }

  property("let info") {
    val repl = Repl()
    await(repl.execute("let a = 5"))
    repl.info("a")    shouldBe "let a: Int"
    repl.info("unit") shouldBe "let unit: Unit"
  }

  property("state reassign") {
    val repl = Repl()
    await(repl.execute("let a = 5"))
    await(repl.execute("1"))
    await(repl.execute("a")) shouldBe Right("res2: Int = 5")
  }

  property("internal decls") {
    val repl = Repl()
    await(repl.execute("func filterStep(acc: List[Int], v: Int) = if (v % 2 == 0) then v :: acc else acc"))
    await(repl.execute("FOLD<5>([1,2,3,4,5], nil, filterStep)"))
    await(repl.execute("FOLD<5>([1,2,3,4,5], nil, filterStep)")) shouldBe Right("res2: List[Int] = [4, 2]")

    repl.info("_isInstanceOf") shouldBe "_isInstanceOf not found in context"
  }

  property("url slash strip") {
    val url = "testnodes.wavesnodes.com"
    val settings = NodeConnectionSettings(url + "///", 'T'.toByte, "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun")
    settings.normalizedUrl shouldBe url
  }

  property("bytevector format display") {
    val repl = Repl()
    await(repl.execute("sha256(base58'')")) shouldBe Right("res1: ByteVector = base58'GKot5hBsd81kMupNCXHaqbhv3huEbxAFMLnpcX2hniwn'")
  }
    
  property("reconfigure") {
    val address1 = "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun"
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, address1)
    val repl = Repl(Some(settings))

    await(repl.execute("let a = 1"))
    await(repl.execute("func inc(a: Int) = a + 1"))
    await(repl.execute("this")) shouldBe Right(
      s"""
         |res1: Address = Address(
         |	bytes = base58'$address1'
         |)
       """.trim.stripMargin
    )

    val address2 = "3PDjjLFDR5aWkKgufika7KSLnGmAe8ueDpC"
    val reconfiguredRepl = repl.reconfigure(settings.copy(address = address2))

    await(reconfiguredRepl.execute("a")) shouldBe Right("res2: Int = 1")
    await(reconfiguredRepl.execute("inc(1)")) shouldBe Right("res3: Int = 2")
    await(reconfiguredRepl.execute("this")) shouldBe Right(
      s"""
         |res4: Address = Address(
         |	bytes = base58'$address2'
         |)
       """.trim.stripMargin
    )
  }

  property("parse error message") {
    val repl = Repl()
    await(repl.execute("FOLD<1>()")) shouldBe Left("Can't parse 'FOLD<1>()'")
    await(repl.execute("getInteger(")) shouldBe Left("Can't parse 'getInteger('")
  }
}
