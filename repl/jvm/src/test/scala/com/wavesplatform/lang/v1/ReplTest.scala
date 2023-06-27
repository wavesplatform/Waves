package com.wavesplatform.lang.v1

import cats.Functor
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.BlockchainUnavailableException
import com.wavesplatform.lang.v1.repl.node.http.NodeClient.ResponseWrapper
import com.wavesplatform.lang.v1.repl.node.http.response.model.HeightResponse
import com.wavesplatform.lang.v1.repl.node.http.{NodeClient, NodeConnectionSettings}
import com.wavesplatform.test.produce
import io.circe.Decoder
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}

class ReplTest extends AnyPropSpec with Matchers {
  def await[A](f: Future[A]): A = Await.result(f, 2 hour)

  property("variable memorization") {
    val repl = Repl()
    await(repl.execute("let a = 1")) shouldBe Right("defined let a: Int")
    await(repl.execute("let b = 2")) shouldBe Right("defined let b: Int")
    await(repl.execute("let c = a + b")) shouldBe Right("defined let c: Int")
    await(repl.execute("c")) shouldBe Right("res1: Int = 3")
    await(repl.execute("a + b")) shouldBe Right("res2: Int = 3")
    await(repl.execute("res1 + res2")) shouldBe Right("res3: Int = 6")
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
    await(repl.execute("""let a = {{1}""")) shouldBe Left("Can't parse 'let a = {{1}'")
    await(repl.execute("""1 %% 2""")) shouldBe Left("Compilation failed: [expected a second operator in 3-3]")
  }

  property("logic errors") {
    val repl = Repl()
    await(repl.execute(""" let a = base64'12345' """)) shouldBe Left("Compilation failed: can't parse Base64 string in 17-21")
    await(repl.execute(""" let b = "abc" + 1 """)) shouldBe Left("Compilation failed: [Can't find a function overload '+'(String, Int) in 9-18]")
  }

  property("exceptions") {
    val repl = Repl()
    val msg  = "error message"
    await(repl.execute(s""" throw("$msg") """)) shouldBe Left(msg)
    await(repl.execute(s""" throw() """)) shouldBe Left("Explicit script termination")
    await(repl.execute(s""" throw("") """)) shouldBe Left("Evaluation error")
  }

  property("waves context funcs absent") {
    val repl = Repl()

    await(repl.execute(s""" transferTransactionById(base58'fdg') """)) should produce("Blockchain state is unavailable from REPL")

    await(repl.execute(s""" let a = 1 """))
    await(repl.execute(s""" a """)) shouldBe Right("res1: Int = 1")
  }

  property("state reset") {
    val repl = Repl()
    await(repl.execute("let a = 1")) shouldBe Right("defined let a: Int")
    await(repl.execute("let b = a + 2")) shouldBe Right("defined let b: Int")
    await(repl.execute("b")) shouldBe Right("res1: Int = 3")
    repl.clear()
    await(repl.execute("a")) shouldBe Left("Compilation failed: [A definition of 'a' is not found in 0-1]")
    await(repl.execute("b")) shouldBe Left("Compilation failed: [A definition of 'b' is not found in 0-1]")
  }

  property("keep state if input contain both expression and declarations") {
    val repl = Repl()
    await(
      repl.execute(
        """
          | func my1() = 3
          | let a = 2
          | let b = 7
          | func my2(a: String) = a + a
          | my1()
      """.stripMargin
      )
    ) shouldBe Right(
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
    await(
      repl.execute(
        """
         func f() = {
           let a = 3
           a
         }
      """
      )
    )
    await(
      repl.execute(
        """
         let b = {
           let a = 3
           a
         }
      """
      )
    )
    await(repl.execute("f()")) shouldBe Right("res1: Int = 3")
    await(repl.execute("b")) shouldBe Right("res2: Int = 3")
    await(repl.execute("a")) shouldBe Left("Compilation failed: [A definition of 'a' is not found in 0-1]")
  }

  property("type info") {
    val repl = Repl()
    repl.info("AttachedPayment") shouldBe "type AttachedPayment { assetId: ByteVector|Unit, amount: Int }"
    repl.info("String") shouldBe "type String"
  }

  property("func info") {
    val repl = Repl()
    repl.info("getInteger").split("\n") shouldBe Array(
      "func getInteger(addressOrAlias: Address|Alias, key: String): Int|Unit",
      "func getInteger(data: List[BinaryEntry|BooleanEntry|DeleteEntry|IntegerEntry|StringEntry], key: String): Int|Unit",
      "func getInteger(data: List[BinaryEntry|BooleanEntry|DeleteEntry|IntegerEntry|StringEntry], index: Int): Int|Unit",
      "func getInteger(key: String): Int|Unit"
    )
    await(repl.execute("func my(a: Int) = toString(a)"))
    repl.info("my") shouldBe "func my(a: Int): String"
  }

  property("let info") {
    val repl = Repl()
    await(repl.execute("let a = 5"))
    repl.info("a") shouldBe "let a: Int"
    repl.info(GlobalValNames.Unit) shouldBe "let unit: Unit"
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
    val url      = "testnodes.wavesnodes.com"
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
    val repl     = Repl(Some(settings))

    await(repl.execute("let a = 1"))
    await(repl.execute("func inc(a: Int) = a + 1"))
    await(repl.execute("this")) shouldBe Right(
      s"""
         |res1: Address = Address(
         |	bytes = base58'$address1'
         |)
       """.trim.stripMargin
    )

    val address2         = "3PDjjLFDR5aWkKgufika7KSLnGmAe8ueDpC"
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

  property("libraries") {
    val address  = "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun"
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, address)
    val repl = Repl(
      Some(settings),
      libraries = List(
        """
          |func f1(a: Int, b: Int) = a * a + b * b
          |func f2(a: Int, b: Int) = a * a - b * b
          |
          |let a = 12345
      """.stripMargin,
        """
          |let b = 678
          |
          |func g1() = this.bytes.toBase58String()
      """.stripMargin
      )
    )
    await(repl.execute("f1(4, 3)")) shouldBe Right("res1: Int = 25")
    await(repl.execute("f2(4, 3)")) shouldBe Right("res2: Int = 7")
    await(repl.execute(""" g1() """)) shouldBe Right(s"""res3: String = "$address"""")
    await(repl.execute("a")) shouldBe Right("res4: Int = 12345")
    await(repl.execute("b")) shouldBe Right("res5: Int = 678")
  }

  property("blockchain interaction using lets from libraries is prohibited") {
    val address  = "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun"
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, address)
    val repl = Repl(
      Some(settings),
      libraries = List("let a = this")
    )
    (the[BlockchainUnavailableException] thrownBy await(repl.execute("a"))).toString shouldBe
      "Blockchain interaction using lets from libraries is prohibited, use functions instead"
  }

  property("addressFromPublicKey function") {
    val address  = "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun"
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, address)
    val repl     = Repl(Some(settings))
    await(repl.execute("addressFromPublicKey(base58'HnU9jfhpMcQNaG5yQ46eR43RnkWKGxerw2zVrbpnbGof')")) shouldBe Right(
      "res1: Address = Address(\n\tbytes = base58'3N7rGHurxjXCPDhJLvLxWQ1YKq1tiUDRKUL'\n)"
    )
    await(
      repl.execute("addressFromPublicKey(base58'1ejb7sZqEyRLXjqukkZLmwP7KCJqbdw74oQQJRnAeir66zFQ56ZC3qP76yBLaW4hZY9NXtZ6LqnUDztZdAmCNqU')")
    ) shouldBe Right(
      "res2: Address = Address(\n\tbytes = base58'3N8tAA42HCeoea6jqF5k3twBYCms5irDqmN'\n)"
    )
  }

  property("transactionHeightById for failed transaction") {
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, "")
    val client = new NodeClient {
      override def get[F[_]: Functor: ResponseWrapper, R: Decoder](path: String): Future[F[R]] = {
        if (path == "/transactions/info/abcd")
          Future.successful(Some(HeightResponse(1, succeed = false)).asInstanceOf[F[R]])
        else
          ???
      }
    }
    val repl = Repl(Some(settings), Some(client))
    await(repl.execute("transactionHeightById(base58'abcd')")) shouldBe Right("res1: Int|Unit = Unit")
  }
}
