package com.wavesplatform.lang.v1

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.v1.repl.Repl
import com.wavesplatform.lang.v1.repl.node.BlockchainUnavailableException
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
    await(repl.execute(""" let a = {{1} """)) shouldBe Left("Compilation failed: expected a value's expression in 9-9")
    await(repl.execute(""" 1 ++ 2 """))       shouldBe Left("Compilation failed: expected a second operator in 4-4")
  }

  property("logic errors") {
    val repl = Repl()
    await(repl.execute(""" let a = base64'12345' """)) shouldBe Left("Compilation failed: can't parse Base64 string in 17-21")
    await(repl.execute(""" let b = "abc" + 1 """))     shouldBe Left("Compilation failed: Can't find a function overload '+'(String, Int) in 9-18")
  }

  property("exceptions") {
    val repl = Repl()
    val msg = "error message"
    await(repl.execute(s""" throw("$msg") """)) shouldBe Left(msg)
  }

  property("waves context funcs absent") {
    val repl = Repl()

    val err = the[BlockchainUnavailableException] thrownBy await(repl.execute(s""" transferTransactionById(base58'fdg') """))
    err.toString shouldBe "Blockchain state is unavailable from REPL"

    await(repl.execute(s""" let a = 1 """))
    await(repl.execute(s""" a """))  shouldBe Right("res1: Int = 1")
  }

  property("state reset") {
    val repl = Repl()
    await(repl.execute("let a = 1"))     shouldBe Right("defined let a: Int")
    await(repl.execute("let b = a + 2")) shouldBe Right("defined let b: Int")
    await(repl.execute("b"))             shouldBe Right("res1: Int = 3")
    repl.clear()
    await(repl.execute("a")) shouldBe Left("Compilation failed: A definition of 'a' is not found in 0-1")
    await(repl.execute("b")) shouldBe Left("Compilation failed: A definition of 'b' is not found in 0-1")
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
    await(repl.execute("a"))   shouldBe Left("Compilation failed: A definition of 'a' is not found in 0-1")
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

  ignore("waves context") {
    val settings = NodeConnectionSettings("testnodes.wavesnodes.com", 'T'.toByte, "3MpLKVSnWSY53bSNTECuGvESExzhV9ppcun")
    val repl = Repl(Some(settings))

    await(repl.execute(""" this.getInteger("int") """))  shouldBe Right("res1: Int|Unit = 100500")
    await(repl.execute(""" this.getString("str") """))   shouldBe Right("res2: String|Unit = text")
    await(repl.execute(""" this.getBinary("bin") """))   shouldBe Right("res3: ByteVector|Unit = r1Mw3j9J")
    await(repl.execute(""" this.getBoolean("bool") """)) shouldBe Right("res4: Boolean|Unit = true")

    await(repl.execute(""" height """)).explicitGet() should fullyMatch regex "res5: Int = \\d+".r

    await(repl.execute(""" transferTransactionById(base58'GgjvCxoDP2FtNrKMqsWrUqJZfMGTiWB1tF2RyYHk6u9w') """)) shouldBe
      Right(
        """
          |res6: TransferTransaction|Unit = TransferTransaction(
          |	recipient = Address(
          |		bytes = base58'3Mp5jDVBB39fpYRpWgsmmc3qXYbDBJQ9r3H'
          |	)
          |	timestamp = 1567608734974
          |	bodyBytes = base58'2sWypZmVFxitSvqYbjEwh5vi81v6A783JczKGHbMKP2DfnTXzo62e431euh9oyfrt9Kvqsf9H9cCDzUGAMzFwJjSnKVkMW7ZBpuKysukkbgRHNrLPyjn3ZhH'
          |	assetId = Unit
          |	feeAssetId = Unit
          |	amount = 1000000000
          |	version = 2
          |	id = base58'GgjvCxoDP2FtNrKMqsWrUqJZfMGTiWB1tF2RyYHk6u9w'
          |	senderPublicKey = base58'9oYuF7V66UNpD2AgYHb6t2j9GYrf3c6hRvwtop6uD6Rx'
          |	attachment = base58''
          |	sender = Address(
          |		bytes = base58'3Myqjf1D44wR8Vko4Tr5CwSzRNo2Vg9S7u7'
          |	)
          |	proofs = [base58'2jBDiyCdj9hpZrjyUNcuCLicc7onjLrz3fXcSbNsRYUUJCVbUZKKRVYthXG2EB2jxGHCfApg4w4qee88HbsVmT9s', base58'', base58'', base58'', base58'', base58'', base58'', base58'']
          |	fee = 100000
          |)
        """.trim.stripMargin
      )

    await(repl.execute(""" transactionHeightById(base58'GgjvCxoDP2FtNrKMqsWrUqJZfMGTiWB1tF2RyYHk6u9w') """)) shouldBe
      Right("res7: Int|Unit = 661401")

    await(repl.execute(""" assetInfo(base58'AMFteLfPzPhTsFc3NfvHG7fSRUnsp3tJXPH88G1PCisT') """)) shouldBe
      Right(
        """
          |res8: Asset|Unit = Asset(
          |	quantity = 100000000000000000
          |	issuer = Address(
          |		bytes = base58'3N5net4nzSeeqxPfGZrvVvnGavsinipQHbE'
          |	)
          |	scripted = false
          |	issuerPublicKey = base58'3NBHih2cqtaywWcDnw6g4PZ4Z4Ug57zdaVz'
          |	id = base58'AMFteLfPzPhTsFc3NfvHG7fSRUnsp3tJXPH88G1PCisT'
          |	decimals = 8
          |	reissuable = true
          |	sponsored = false
          |)
        """.trim.stripMargin
      )

    await(repl.execute(""" blockInfoByHeight(662371) """)) shouldBe
      Right(
        """
          |res9: BlockInfo|Unit = BlockInfo(
          |	baseTarget = 1456
          |	generator = Address(
          |		bytes = base58'3Mp6Lhe3sN97xtuFSunG7TCgVVh7QGoxzmu'
          |	)
          |	timestamp = 1567666635819
          |	height = 662371
          |	generationSignature = base58'BVGVtzt3wt646ECcaDV5ne8QcgsKs2vVQmJe8YSMkSHs'
          |	generatorPublicKey = base58'3NB1Yz7fH1bJ2gVDjyJnuyKNTdMFARkKEpV'
          |)
        """.trim.stripMargin
      )

    await(repl.execute(
      """ addressFromRecipient(Alias("aaaa")) ==
          addressFromRecipient(Address(base58'3N9bnz3AtjeC1p92CR7jFkTnv9PZjtoPkMQ'))
      """
    )) shouldBe
      Right("res10: Boolean = true")

    await(repl.execute(
      """ assetBalance(
            Address(base58'3Mrhtzv9KEtjx4mG47oxgjahHKW33oTntEV'),
            base58'HUdXNRE4VcCx64PCPYwh6KL2cxvRaKcR8bXe3Ar9fG4p'
          )
       """
    )).explicitGet() should fullyMatch regex "res11: Int = \\d+".r

    await(repl.execute(""" this.wavesBalance() """)).explicitGet() should fullyMatch regex "res12: Int = \\d+".r
  }
}
