package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.*
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.utils.getDecompilerContext
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.*
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.BinaryOperation.NE_OP
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.test.PropSpec
import org.scalatest.Assertion

class DecompilerTest extends PropSpec {

  val sp = "\\s+".r
  implicit class StringCmp(s1: String) {
    def shouldEq(s2: String) = sp.replaceAllIn(s1, "") shouldEqual sp.replaceAllIn(s2, "")
  }

  val decompilerContextV3 = getTestContext(V3).decompilerContext
  val decompilerContextV4 = getTestContext(V4).decompilerContext

  private def assertDecompile(script: String, decompiled: String, version: StdLibVersion): Assertion = {
    val expr   = TestCompiler(version).compileExpression(script.stripMargin).expr
    val result = Decompiler(expr, getDecompilerContext(version, Expression))
    result shouldBe decompiled.stripMargin.trim
  }

  property("successful on very deep expressions (stack overflow check)") {
    val expr = (1 to 10000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(function = FunctionHeader.Native(100), List(CONST_LONG(1), acc))
    }
    Decompiler(expr, decompilerContextV3) should startWith("(1 + (1 + (1 + (1 + (1 + (1 + ")
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), Terms.LET_BLOCK(LET("b", CONST_LONG(2)), Terms.LET_BLOCK(LET("c", CONST_LONG(3)), TRUE)))
    Decompiler(expr, decompilerContextV3) shouldEq
      """let a = 1
        |let b = 2
        |let c = 3
        |true""".stripMargin
  }

  property("let in let") {
    val expr =
      Terms.LET_BLOCK(LET("a", Terms.LET_BLOCK(LET("x", CONST_LONG(0)), TRUE)), Terms.LET_BLOCK(LET("c", CONST_LONG(3)), TRUE))
    Decompiler(expr, decompilerContextV3) shouldEq
      """let a = {
        |    let x = 0
        |    true
        |    }
        |let c = 3
        |true""".stripMargin
  }
  property("native function call with one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(500),
      args = List(TRUE)
    )
    Decompiler(expr, decompilerContextV3) shouldEq "sigVerify(true)"
  }

  property("native function call with two arg (binary operations)") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContextV3) shouldEq "(1 + 2)"
  }

  property("nested binary operations") {
    val expr = FUNCTION_CALL(Native(105), List(FUNCTION_CALL(Native(101), List(REF("height"), REF("startHeight"))), REF("interval")))
    Decompiler(expr, decompilerContextV3) shouldEq "((height - startHeight) / interval)"
  }

  property("unknown native function call") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(254),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContextV3) shouldEq "Native<254>(1, 2)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, decompilerContextV3) shouldEq "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, decompilerContextV3) shouldEq "foo()"
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    val actual = Decompiler(expr, decompilerContextV3)
    val expected = """|let vari = p
                      |true""".stripMargin
    actual shouldEq expected
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("v", CONST_LONG(1)),
      Terms.FUNCTION_CALL(
        function = FunctionHeader.Native(100),
        args = List(REF("v"), CONST_LONG(2))
      )
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """let v = 1
        |(v + 2)""".stripMargin
  }

  property("neq binary op") {
    val expr =
      Terms.FUNCTION_CALL(
        function = FunctionHeader.User(NE_OP.func),
        args = List(CONST_LONG(4), CONST_LONG(2))
      )
    Decompiler(expr, decompilerContextV3) shouldEq
      """(4 != 2)""".stripMargin
  }

  property("function with complex args") {
    val expr = BLOCK(
      LET(
        "x",
        BLOCK(
          LET(
            "y",
            Terms.FUNCTION_CALL(
              function = FunctionHeader.User("foo"),
              args = List(BLOCK(LET("a", CONST_LONG(4)), REF("a")), CONST_LONG(2))
            )
          ),
          TRUE
        )
      ),
      FALSE
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """let x = {
        |    let y = foo({
        |        let a = 4
        |        a
        |        }, 2)
        |    true
        |    }
        |false""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET(
        "p",
        Terms.BLOCK(Terms.LET("v", CONST_LONG(1)), Terms.FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("v"), CONST_LONG(2))))
      ),
      Terms.FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("p"), CONST_LONG(3)))
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """let p = {
        |    let v = 1
        |    (v + 2)
        |    }
        |(p + 3)""".stripMargin
  }

  property("old match") {
    val expr = Terms.BLOCK(
      LET("v", CONST_LONG(1)),
      IF(
        IF(
          FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("v"), CONST_LONG(2))),
          TRUE,
          FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("v"), CONST_LONG(3)))
        ),
        BLOCK(LET("p", REF("v")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """let v = 1
        |if (if ((v + 2))
        |    then true
        |    else (v + 3))
        |    then {
        |        let p = v
        |        true
        |        }
        |    else false""".stripMargin
  }

  property("ref getter idents") {
    val expr = GETTER(REF("a"), "foo")
    Decompiler(expr, decompilerContextV3) shouldEq
      """a.foo""".stripMargin
  }

  property("block getter idents") {
    val expr = GETTER(BLOCK(LET("a", FALSE), REF("a")), "foo")
    Decompiler(expr, decompilerContextV3) shouldEq
      """(
        |    let a = false
        |    a
        |    ).foo""".stripMargin
  }

  property("Invoke contract with verifier decompilation") {
    val contract = DApp(
      DAppMeta(),
      List(FUNC("foo", List(), FALSE), FUNC("bar", List(), IF(FUNCTION_CALL(User("foo"), List()), TRUE, FALSE))),
      List(
        CallableFunction(
          CallableAnnotation("invocation"),
          FUNC(
            "baz",
            List("a"),
            BLOCK(
              LET("x", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              IF(
                FUNCTION_CALL(User("foo"), List()),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("b").explicitGet(), CONST_LONG(1))),
                        FUNCTION_CALL(
                          Native(1100),
                          List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), REF("x"))), REF(GlobalValNames.Nil))
                        )
                      )
                    )
                  )
                ),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a").explicitGet(), REF("a"))),
                        FUNCTION_CALL(
                          Native(1100),
                          List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), REF("x"))), REF(GlobalValNames.Nil))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
    )
    Decompiler(contract: DApp, decompilerContextV3, V3) shouldEq
      """|func foo () = false
         |
         |
         |func bar () = if (foo())
         |    then true
         |    else false
         |
         |
         |@Callable(invocation)
         |func baz (a) = {
         |    let x = invocation.caller.bytes
         |    if (foo())
         |        then WriteSet([DataEntry("b", 1), DataEntry("sender", x)])
         |        else WriteSet([DataEntry("a", a), DataEntry("sender", x)])
         |    }
         |
         |
         |@Verifier(t)
         |func verify () = true
         |""".stripMargin
  }

  property("Invoke contract decompilation") {
    val contract = DApp(
      DAppMeta(),
      List(Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))),
      List(
        CallableFunction(
          CallableAnnotation("i"),
          Terms.FUNC(
            "testfunc",
            List("amount"),
            BLOCK(
              LET("pmt", CONST_LONG(1)),
              TRUE
            )
          )
        )
      ),
      None
    )
    Decompiler(contract, decompilerContextV3, V3) shouldEq
      """func foo (bar,buz) = true
        |
        |
        |@Callable(i)
        |func testfunc (amount) = {
        |    let pmt = 1
        |    true
        |    }
        |
        |""".stripMargin

  }

  property("Invoke contract decompilation with meta") {
    val contract = DApp(
      DAppMeta(),
      List(Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))),
      List(
        CallableFunction(
          CallableAnnotation("i"),
          Terms.FUNC(
            "testfunc",
            List("amount"),
            BLOCK(
              LET("pmt", CONST_LONG(1)),
              TRUE
            )
          )
        )
      ),
      None
    )
    Decompiler(contract, decompilerContextV3, V3) shouldEq
      """func foo (bar,buz) = true
        |
        |
        |@Callable(i)
        |func testfunc (amount) = {
        |    let pmt = 1
        |    true
        |    }
        |
        |""".stripMargin

  }

  property("bytestring") {
    val test = Base58.encode("abc".getBytes("UTF-8"))
    // ([REVIEW]: may be i`am make a mistake here)
    val expr = Terms.BLOCK(Terms.LET("param", CONST_BYTESTR(ByteStr(test.getBytes("UTF-8"))).explicitGet()), REF("param"))
    Decompiler(expr, decompilerContextV3) shouldEq
      """let param = base58'3K3F4C'
        |param""".stripMargin
  }

  property("getter") {
    val expr = Terms.GETTER(
      Terms.FUNCTION_CALL(
        function = FunctionHeader.User("testfunc"),
        args = List(TRUE)
      ),
      "testfield"
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """testfunc(true).testfield""".stripMargin
  }

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX").explicitGet())
    Decompiler(expr, decompilerContextV3) shouldEq
      """if (true)
        |    then 1
        |    else "XXX"""".stripMargin
  }

  property("if expression") {
    val expr1 = FUNCTION_CALL(Native(101), List(IF(TRUE, CONST_LONG(1), CONST_LONG(2)), CONST_LONG(3)))
    Decompiler(expr1, decompilerContextV3) shouldEq
      """((if (true)
        |    then 1
        |    else 2) - 3)""".stripMargin
    val expr2 = FUNCTION_CALL(Native(101), List(CONST_LONG(3), IF(TRUE, CONST_LONG(1), CONST_LONG(2))))
    Decompiler(expr2, decompilerContextV3) shouldEq
      """(3 - (if (true)
        |    then 1
        |    else 2))""".stripMargin
    val expr3 = GETTER(IF(TRUE, CONST_LONG(1), CONST_LONG(2)), "foo")
    Decompiler(expr3, decompilerContextV3) shouldEq
      """(if (true)
        |    then 1
        |    else 2
        |    ).foo""".stripMargin
    val expr4 = FUNCTION_CALL(Native(401), List(IF(TRUE, REF(GlobalValNames.Nil), REF(GlobalValNames.Nil)), CONST_LONG(0)))
    Decompiler(expr4, decompilerContextV3) shouldEq
      """(if (true)
        |    then nil
        |    else nil)[0]""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX").explicitGet()))
    Decompiler(expr, decompilerContextV3) shouldEq
      """if (true)
        |    then 1
        |    else if (true)
        |        then 1
        |        else "XXX"""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE, IF(TRUE, CONST_LONG(1), CONST_STRING("XXX").explicitGet()), CONST_LONG(1))
    Decompiler(expr, decompilerContextV3) shouldEq
      """if (true)
        |    then if (true)
        |        then 1
        |        else "XXX"
        |    else 1""".stripMargin
  }

  property("if with != condition") {
    val expr = IF(
      FUNCTION_CALL(User("!="), List(REF("left"), REF("right"))),
      REF("left"),
      REF("right")
    )
    Decompiler(expr, decompilerContextV3) shouldEq
      """if ((left != right))
        |    then left
        |    else right""".stripMargin
  }

  property("Surge smart accet") {
    val expr = BLOCK(
      LET("startHeight", CONST_LONG(1375557)),
      BLOCK(
        LET("startPrice", CONST_LONG(100000)),
        BLOCK(
          LET("interval", FUNCTION_CALL(Native(104), List(CONST_LONG(24), CONST_LONG(60)))),
          BLOCK(
            LET("exp", FUNCTION_CALL(Native(104), List(FUNCTION_CALL(Native(104), List(CONST_LONG(100), CONST_LONG(60))), CONST_LONG(1000)))),
            BLOCK(
              LET("$match0", REF("tx")),
              IF(
                FUNCTION_CALL(Native(1), List(REF("$match0"), CONST_STRING("ExchangeTransaction").explicitGet())),
                BLOCK(
                  LET("e", REF("$match0")),
                  BLOCK(
                    LET(
                      "days",
                      FUNCTION_CALL(Native(105), List(FUNCTION_CALL(Native(101), List(REF("height"), REF("startHeight"))), REF("interval")))
                    ),
                    IF(
                      IF(
                        IF(
                          FUNCTION_CALL(
                            Native(103),
                            List(
                              GETTER(REF("e"), "price"),
                              FUNCTION_CALL(
                                Native(104),
                                List(
                                  REF("startPrice"),
                                  FUNCTION_CALL(Native(100), List(CONST_LONG(1), FUNCTION_CALL(Native(104), List(REF("days"), REF("days")))))
                                )
                              )
                            )
                          ),
                          FUNCTION_CALL(
                            User("!"),
                            List(FUNCTION_CALL(User("isDefined"), List(GETTER(GETTER(GETTER(REF("e"), "sellOrder"), "assetPair"), "priceAsset"))))
                          ),
                          FALSE
                        ),
                        FUNCTION_CALL(
                          Native(103),
                          List(
                            REF("exp"),
                            FUNCTION_CALL(
                              Native(101),
                              List(GETTER(GETTER(REF("e"), "sellOrder"), "expiration"), GETTER(GETTER(REF("e"), "sellOrder"), "timestamp"))
                            )
                          )
                        ),
                        FALSE
                      ),
                      FUNCTION_CALL(
                        Native(103),
                        List(
                          REF("exp"),
                          FUNCTION_CALL(
                            Native(101),
                            List(GETTER(GETTER(REF("e"), "buyOrder"), "expiration"), GETTER(GETTER(REF("e"), "buyOrder"), "timestamp"))
                          )
                        )
                      ),
                      FALSE
                    )
                  )
                ),
                IF(
                  FUNCTION_CALL(
                    Native(1),
                    List(REF("$match0"), CONST_STRING("BurnTransaction").explicitGet())
                  ),
                  BLOCK(LET("tx", REF("$match0")), TRUE),
                  FALSE
                )
              )
            )
          )
        )
      )
    )

    Decompiler(expr, decompilerContextV3) shouldEq
      """let startHeight = 1375557
        |let startPrice = 100000
        |let interval = (24 * 60)
        |let exp = ((100 * 60) * 1000)
        |match tx {
        |    case e: ExchangeTransaction => 
        |        let days = ((height - startHeight) / interval)
        |        if (if (if ((e.price >= (startPrice * (1 + (days * days)))))
        |            then !(isDefined(e.sellOrder.assetPair.priceAsset))
        |            else false)
        |            then (exp >= (e.sellOrder.expiration - e.sellOrder.timestamp))
        |            else false)
        |            then (exp >= (e.buyOrder.expiration - e.buyOrder.timestamp))
        |            else false
        |    case tx: BurnTransaction => 
        |        true
        |    case _ => 
        |        false
        |}""".stripMargin
  }

  def compileExpr(code: String, v: StdLibVersion = V3): Either[String, (EXPR, TYPE)] = {
    val untyped = Parser.parseExpr(code).get.value
    val typed   = ExpressionCompiler(getTestContext(v).compilerContext, v, untyped)
    typed
  }

  property("match") {
    val script = """
      match tv {
        case x : PointA|PointB => x
        case x : PointC => x
    }"""

    val expr = compileExpr(script).explicitGet()._1

    val rev = Decompiler(expr, decompilerContextV3)

    rev shouldEq """match tv {
                   |    case x: PointB|PointA => 
                   |        x
                   |    case x: PointC => 
                   |        x
                   |    case _ => 
                   |        throw("Match error")
                   |}""".stripMargin
  }

  property("match with case without type") {
    val script = """
      match tv {
        case _: PointA|PointB => 1
        case _ => 2
    }"""

    val expr = compileExpr(script).explicitGet()._1

    val rev = Decompiler(expr, decompilerContextV3)

    rev shouldEq """match tv {
                   |    case _: PointB|PointA =>
                   |        1
                   |    case _ =>
                   |        2
                   |}""".stripMargin
  }

  property("match with case without var") {
    val script = """
      match tv {
        case _ : PointA|PointB => 1
        case x => x
    }"""

    val expr = compileExpr(script).explicitGet()._1

    val rev = Decompiler(expr, decompilerContextV3)

    rev shouldEq """match tv {
                   |    case _: PointB|PointA => 
                   |        1
                   |    case x => 
                   |        x
                   |}""".stripMargin
  }

  property("multiple value list") {
    val script = """["a", "b", "c", "d"]"""
    val expr   = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("concat with nil is hidden") {
    val expected = """["a", "b"]"""
    val expr1    = compileExpr(""""a"::"b"::nil""").explicitGet()._1
    val expr2    = compileExpr("""cons("a", cons("b", nil))""").explicitGet()._1
    Decompiler(expr1, decompilerContextV3) shouldEq expected
    Decompiler(expr2, decompilerContextV3) shouldEq expected
  }

  property("single value list") {
    val script = """["a"]"""
    val expr   = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("existing list concat") {
    val script =
      """let list = ["b", "c", "d"]
        |"a" :: list""".stripMargin
    val expr = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("extracted functions") {
    val script = """addressFromStringValue("abcd")"""
    val expr   = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("list element access") {
    val script =
      """let arr = [1, 2, 3]
        |arr[1]""".stripMargin
    val expr = compileExpr(script).explicitGet()._1
    val expr2 = compileExpr(
      """let arr = [1, 2, 3]
        |arr.getElement(1)""".stripMargin
    ).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
    Decompiler(expr2, decompilerContextV3) shouldEq script
  }

  property("get state values") {
    val script =
      """let bin = getBinary(this, "key")
        |let boo = getBoolean(this, "key")
        |let int = getInteger(this, "key")
        |let str = getString(this, "key")
        |let binV = getBinaryValue(this, "key")
        |let booV = getBooleanValue(this, "key")
        |let intV = getIntegerValue(this, "key")
        |let strV = getStringValue(this, "key")
        |boo""".stripMargin
    val expr = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("get DataEntry list values by key") {
    val script =
      """let list = [DataEntry("k1", fromBase58String("3ye1XJ")), DataEntry("k2", true), DataEntry("k3", 42), DataEntry("k4", "true")]
        |let bin = getBinary(list, "k1")
        |let boo = getBoolean(list, "k2")
        |let int = getInteger(list, "k3")
        |let str = getString(list, "k4")
        |let binV = getBinaryValue(list, "k1")
        |let booV = getBooleanValue(list, "k2")
        |let intV = getIntegerValue(list, "k3")
        |let strV = getStringValue(list, "k4")
        |boo""".stripMargin
    val expr = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("get DataEntry list values by index") {
    val script =
      """let list = [DataEntry("k1", fromBase58String("3ye1XJ")), DataEntry("k2", true), DataEntry("k3", 42), DataEntry("k4", "true")]
        |let bin = getBinary(list, 0)
        |let boo = getBoolean(list, 1)
        |let int = getInteger(list, 2)
        |let str = getString(list, 3)
        |let binV = getBinaryValue(list, 0)
        |let booV = getBooleanValue(list, 1)
        |let intV = getIntegerValue(list, 2)
        |let strV = getStringValue(list, 3)
        |boo""".stripMargin
    val expr = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("V3 - checkMerkleProof") {
    val script =
      "checkMerkleProof(base58'',base58'',base58'')"
    val expr = compileExpr(script, v = V3).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq script
  }

  property("list func params") {
    val script =
      """
        | func f(a: List[String], b: String) = a[0] + b
        | f(["a", "b"], "c")
      """.stripMargin
    val expr = compileExpr(script).explicitGet()._1
    Decompiler(expr, decompilerContextV3) shouldEq
      """func f (a,b) = (a[0] + b)
        |
        |f(["a", "b"], "c")""".stripMargin
  }

  property("V4 - contains") {
    val script = """"abc".contains("b")"""
    val expr   = compileExpr(script, V4).explicitGet()._1
    val res    = Decompiler(expr, decompilerContextV4)
    res shouldEq """contains("abc", "b")"""
  }

  property("V4 - valueOrElse") {
    val script = """let a = 1
                   |let b = 2
                   |a.valueOrElse(b)""".stripMargin
    val expr = compileExpr(script, V4).explicitGet()._1
    val res  = Decompiler(expr, decompilerContextV4)
    res shouldEq """let a = 1
                   |let b = 2
                   |valueOrElse(a, b)""".stripMargin
  }

  property("V4 - listAppend (:+)") {
    val script = """[1, 2, 3] :+ 4"""
    val expr   = compileExpr(script, V4).explicitGet()._1
    val res    = Decompiler(expr, decompilerContextV4)
    res shouldEq """([1, 2, 3] :+ 4)"""
  }

  property("V4 - listConcat (++)") {
    val script = """[1, 2, 3] ++ [4, 5, 42]"""
    val expr   = compileExpr(script, V4).explicitGet()._1
    val res    = Decompiler(expr, decompilerContextV4)
    res shouldEq """([1, 2, 3] ++ [4, 5, 42])"""
  }

  property("V4 - median") {
    val script = """median([1, 2, 3]) + [1, 2, 3].median()"""
    val expr   = compileExpr(script, V4).explicitGet()._1
    val res    = Decompiler(expr, decompilerContextV4)
    res shouldEq """(median([1, 2, 3]) + median([1, 2, 3]))"""
  }

  property("V4 - new contract result format") {
    val directives =
      """
        | {-# STDLIB_VERSION 4    #-}
        | {-#CONTENT_TYPE    DAPP #-}
        |""".stripMargin
    val script =
      """
        | @Callable(i)
        | func foo() =
        |   [
        |     IntegerEntry("key", 1),
        |     BooleanEntry("key", true),
        |     StringEntry("key", "str"),
        |     BinaryEntry("key", base58''),
        |     BinaryEntry("key", calculateAssetId(Issue("name", "description", 1000, 4, true, unit, 0))),
        |     DeleteEntry("key"),
        |     ScriptTransfer(i.caller, 1, base58''),
        |     Issue("name", "description", 1000, 4, true, unit, 0),
        |     Reissue(base58'', 1, false),
        |     Burn(base58'', 1),
        |     SponsorFee(base58'', unit)
        |   ]
        """.stripMargin

    val parsedExpr = Parser.parseContract(directives ++ script).get.value

    val ctx =
      Monoid.combine(
        PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment],
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V4).explicitGet()
    val res  = Decompiler(dApp, decompilerContextV4, V4)
    res shouldEq script
  }

  property("V4 - new functions") {
    val sizes  = Seq(16, 32, 64, 128)
    val hashes = Seq("blake2b", "keccak", "sha")
    val directives =
      """
        | {-# STDLIB_VERSION 4    #-}
        | {-#CONTENT_TYPE    DAPP #-}
        |""".stripMargin
    val script =
      s"""
         | @Callable(i)
         | func foo() = {
         |   let v1 = transferTransactionFromProto(base58'')
         |   let v2 = groth16Verify(base58'', base58'', base58'')
         |   let v3 = createMerkleRoot(nil, base58'', 0)
         |   let v4 = [${(for { s <- sizes; h <- hashes } yield h ++ "256_" ++ s.toString ++ "Kb(base58'')").mkString(", ")}]
         |   let v5 = [${(for { s <- sizes } yield "sigVerify_" ++ s.toString ++ "Kb(base58'', base58'', base58'')").mkString(", ")}]
         |   let v6 = [${(for { s <- sizes } yield "rsaVerify_" ++ s.toString ++ "Kb(SHA256, base58'', base58'', base58'')").mkString(", ")}]
         |   let v7 = [${(for { s <- 1 to 15 } yield "groth16Verify_" ++ s.toString ++ "inputs( base58'', base58'', base58'')").mkString(", ")}]
         |   let v8 = value(1)
         |   let v9 = valueOrErrorMessage(1,"")
         |   let v10 = toUtf8String(base58'')
         |   let v11 = toInt(base58'', toInt(base58''))
         |   let v12 = indexOf("", "")
         |   let v13 = lastIndexOf("", "")
         |   let v14 = split("", "")
         |   let v15 = parseInt("")
         |   let v16 = parseIntValue("")
         |   let v17 = pow(0,0,0,0,0,UP)
         |   let v18 = log(0,0,0,0,0,UP)
         |   let v19 = assetInfo(base58'')
         |   let v20 = blockInfoByHeight(0)
         |   let v21 = transferTransactionById(base58'')
         |   let v22 = toString(Address(base58''))
         |   let v23 = toBase16String(base58'')
         |   let v24 = fromBase16String("")
         |   let v25 = indexOf(["a", "b", "c"], "a")
         |   let v26 = lastIndexOf(["a", "b", "c"], "a")
         |   let v27 = containsElement(["a", "b", "c"], "a")
         |   let v28 = min([1, 2, 3])
         |   let v29 = max([1, 2, 3])
         |   let v30 = makeString(["a","b","c"],"|")
         |   let v31 = ecrecover(base58'aaaa', base58'bbbb')
         |   nil
         | }
        """.stripMargin

    val parsedExpr = Parser.parseContract(directives ++ script).get.value

    val ctx =
      Monoid.combineAll(
        Seq(
          PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, V4).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)
        )
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V4).explicitGet()
    val res  = Decompiler(dApp, ctx.decompilerContext, V4)
    res shouldEq script
  }

  property("V4 - new case types") {
    val directives =
      """
        | {-# STDLIB_VERSION 4    #-}
        | {-#CONTENT_TYPE    DAPP #-}
        |""".stripMargin

    val types =
      ": UpdateAssetInfoTransaction | InvokeScriptTransaction | DataTransaction | IssueTransaction | TransferTransaction | MassTransferTransaction | Asset | BlockInfo"

    def script(t: String) = s"""
                               | func m (v$t) =
                               |   match v {
                               |    case _: UpdateAssetInfoTransaction => nil
                               |    case _: InvokeScriptTransaction => nil
                               |    case _: DataTransaction => nil
                               |    case _: IssueTransaction => nil
                               |    case _: TransferTransaction => nil
                               |    case _: MassTransferTransaction => nil
                               |    case _: Asset => nil
                               |    case _: BlockInfo => nil
                               |    case _ => nil
                               |   }
                               |""".stripMargin

    val parsedExpr = Parser.parseContract(directives ++ script(types)).get.value

    val ctx =
      Monoid.combineAll(
        Seq(
          PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, V4).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)
        )
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V4).explicitGet()
    val res  = Decompiler(dApp, ctx.decompilerContext, V4)
    res shouldEq script("")
  }

  property("V5 - new functions") {
    val directives =
      """
        | {-# STDLIB_VERSION 5    #-}
        | {-#CONTENT_TYPE    DAPP #-}
        |""".stripMargin

    val script =
      s"""
         | @Callable(i)
         | func foo() = {
         |   let v1 = scriptHash(Address(base58''))
         |   nil
         | }
        """.stripMargin

    val parsedExpr = Parser.parseContract(directives ++ script).get.value

    val ctx =
      Monoid.combineAll(
        Seq(
          PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, V5).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet(), fixBigScriptField = true)
        )
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V5).explicitGet()
    val res  = Decompiler(dApp, ctx.decompilerContext, V5)
    res shouldEq script
  }

  property("V5 - new case types") {
    val directives =
      """
        | {-# STDLIB_VERSION 5    #-}
        | {-#CONTENT_TYPE    DAPP #-}
      """.stripMargin

    val types = ": BigInt"

    def script(paramTypes: String) =
      s"""
         | func m (v$paramTypes) =
         |   match v {
         |     case _$types => 0
         |     case _       => 0
         |   }
       """.stripMargin

    val parsedExpr = Parser.parseContract(directives ++ script(types)).get.value

    val ctx =
      Monoid.combineAll(
        Seq(
          PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, V5).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet(), fixBigScriptField = true)
        )
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V5).explicitGet()
    val res  = Decompiler(dApp, ctx.decompilerContext, V5)
    res shouldEq script("")
  }

  property("compacted script") {
    val directives =
      """
        | {-# STDLIB_VERSION 5    #-}
        | {-#CONTENT_TYPE    DAPP #-}
      """.stripMargin

    val script =
      """
        | let fooVar = 42
        |
        | func barFunc(barFuncArg1: Int) = (100500 + barFuncArg1)
        |
        | @Callable(invocation)
        | func bazCallableFunc(bazCallableFuncArg1: Int, bazCallableFuncArg2: String) = {
        |   let result = (barFunc(fooVar) + bazCallableFuncArg1)
        |   [
        |     IntegerEntry("integerEntryKey", result),
        |     StringEntry("stringEntryKey", bazCallableFuncArg2)
        |   ]
        | }
        |
        """.stripMargin

    val scriptWithoutTypes =
      script
        .replace(": Int", "")
        .replace(": String", "")

    val parsedExpr = Parser.parseContract(directives ++ script).get.value

    val ctx =
      Monoid.combineAll(
        Seq(
          PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(Global, V5).withEnvironment[Environment],
          WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet(), fixBigScriptField = true)
        )
      )

    val dApp = compiler.ContractCompiler(ctx.compilerContext, parsedExpr, V5, needCompaction = true).explicitGet()
    val res  = Decompiler(dApp, ctx.decompilerContext, V5)
    res shouldEq scriptWithoutTypes
  }

  property("BigInt unary minus") {
    assertDecompile(
      """
        |let a = -toBigInt(1)
        |true
      """,
      """
        |let a = -(toBigInt(1))
        |true
      """,
      V5
    )
  }

  property("type cast") {
    val script =
      s"""
         |func f() = true
         |func g() = f().as[Boolean]
         |let a    = g().exactAs[Boolean] && f().exactAs[Boolean]
         |a.as[Boolean]
       """
    val decompiledV5 =
      s"""
         |func f () = true
         |
         |func g () = {
         |    let @ = f()
         |    if ($$isInstanceOf(@, "Boolean"))
         |        then @
         |        else unit
         |    }
         |
         |let a = if ({
         |    let @ = g()
         |    if ($$isInstanceOf(@, "Boolean"))
         |        then @
         |        else throw("Couldn't cast Boolean|Unit to Boolean")
         |    })
         |    then {
         |        let @ = f()
         |        if ($$isInstanceOf(@, "Boolean"))
         |            then @
         |            else throw("Couldn't cast Boolean to Boolean")
         |        }
         |    else false
         |let @ = a
         |if ($$isInstanceOf(@, "Boolean"))
         |    then @
         |    else unit
       """
    val decompiledV6 =
      s"""
         |func f () = true
         |
         |func g () = {
         |    let @ = f()
         |    if ($$isInstanceOf(@, "Boolean"))
         |        then @
         |        else unit
         |    }
         |
         |let a = if ({
         |    let @ = g()
         |    if ($$isInstanceOf(@, "Boolean"))
         |        then @
         |        else throw(($$getType(@) + " couldn't be cast to Boolean"))
         |    })
         |    then {
         |        let @ = f()
         |        if ($$isInstanceOf(@, "Boolean"))
         |            then @
         |            else throw(($$getType(@) + " couldn't be cast to Boolean"))
         |        }
         |    else false
         |let @ = a
         |if ($$isInstanceOf(@, "Boolean"))
         |    then @
         |    else unit
       """

    assertDecompile(script, decompiledV5, V5)
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V6)
      .foreach(assertDecompile(script, decompiledV6, _))
  }

  property("calculateDelay()") {
    val script = "calculateDelay(base58'aaa', Address(base58'bbb'), 456)"
    assertDecompile(script, script, V8)
  }

  property("replaceByIndex()") {
    val script = """replaceByIndex(["a", "b", "c"], 1, "x")"""
    assertDecompile(script, script, V8)
  }

  property("escaping characters in string") {
    val script =
      s"""
         |let a = "aaa\\"qqq\\"aaa"
         |let b = "aaa\\\\qqq\\\\aaa"
         |true
       """
    assertDecompile(script, script, V6)
  }
}
