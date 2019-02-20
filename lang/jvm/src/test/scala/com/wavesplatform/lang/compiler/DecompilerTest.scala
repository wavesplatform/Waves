package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.BinaryOperation.NE_OP
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  implicit class StringCmp(s1: String) {
    def shouldEq(s2: String) = s1.replace("\r\n", "\n") shouldEqual s2.replace("\r\n", "\n")
  }

  val CTX: CTX =
    Monoid.combineAll(Seq(PureContext.build(com.wavesplatform.lang.StdLibVersion.V3), CryptoContext.build(Global)))

  val decompilerContext = CTX.decompilerContext

  property("successful on very deep expressions (stack overflow check)") {
    val expr = (1 to 10000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(function = FunctionHeader.Native(100), List(CONST_LONG(1), acc))
    }
    Decompiler(expr, decompilerContext) should startWith("(1 + (1 + (1 + (1 + (1 + (1 + ")
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), Terms.LET_BLOCK(LET("b", CONST_LONG(2)), Terms.LET_BLOCK(LET("c", CONST_LONG(3)), TRUE)))
    Decompiler(expr, decompilerContext) shouldEq
      """let a = 1
        |let b = 2
        |let c = 3
        |true""".stripMargin
  }

  property("let in let") {
    val expr =
      Terms.LET_BLOCK(LET("a", Terms.LET_BLOCK(LET("x", CONST_LONG(0)), TRUE)), Terms.LET_BLOCK(LET("c", CONST_LONG(3)), TRUE))
    Decompiler(expr, decompilerContext) shouldEq
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
    Decompiler(expr, decompilerContext) shouldEq "sigVerify(true)"
  }

  property("native function call with two arg (binary operations)") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContext) shouldEq "(1 + 2)"
  }

  property("nested binary operations") {
    val expr = FUNCTION_CALL(Native(105), List(FUNCTION_CALL(Native(101), List(REF("height"), REF("startHeight"))), REF("interval")))
    Decompiler(expr, decompilerContext) shouldEq "((height - startHeight) / interval)"
  }

  property("unknown native function call") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(254),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContext) shouldEq "Native<254>(1, 2)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, decompilerContext) shouldEq "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, decompilerContext) shouldEq "foo()"
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    val actual   = Decompiler(expr, decompilerContext)
    val expected = """|let vari = p
                      |true""".stripMargin
    actual shouldEq expected
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(Terms.LET("v", CONST_LONG(1)),
                           Terms.FUNCTION_CALL(
                             function = FunctionHeader.Native(100),
                             args = List(REF("v"), CONST_LONG(2))
                           ))
    Decompiler(expr, decompilerContext) shouldEq
      """let v = 1
        |(v + 2)""".stripMargin
  }

  ignore("neq binary op") {
    val expr =
      Terms.FUNCTION_CALL(
        function = FunctionHeader.User(NE_OP.func),
        args = List(CONST_LONG(4), CONST_LONG(2))
      )
    Decompiler(expr, decompilerContext) shouldEq
      """4 != 2""".stripMargin
  }

  property("function with complex args") {
    val expr = BLOCK(
      LET(
        "x",
        BLOCK(LET("y",
                  Terms.FUNCTION_CALL(
                    function = FunctionHeader.User("foo"),
                    args = List(BLOCK(LET("a", CONST_LONG(4)), REF("a")), CONST_LONG(2))
                  )),
              TRUE)
      ),
      FALSE
    )
    Decompiler(expr, decompilerContext) shouldEq
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
        Terms.BLOCK(Terms.LET("v", CONST_LONG(1)), Terms.FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("v"), CONST_LONG(2))))),
      Terms.FUNCTION_CALL(function = FunctionHeader.Native(100), args = List(REF("p"), CONST_LONG(3)))
    )
    Decompiler(expr, decompilerContext) shouldEq
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
    Decompiler(expr, decompilerContext) shouldEq
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
    Decompiler(expr, decompilerContext) shouldEq
      """a.foo""".stripMargin
  }

  property("block getter idents") {
    val expr = GETTER(BLOCK(LET("a", FALSE), REF("a")), "foo")
    Decompiler(expr, decompilerContext) shouldEq
      """{
        |    let a = false
        |    a
        |    }.foo""".stripMargin
  }

  property("Invoke contract with verifier decompilation") {
    val contract = Contract(
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
                  List(FUNCTION_CALL(
                    Native(1100),
                    List(
                      FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("b"), CONST_LONG(1))),
                      FUNCTION_CALL(Native(1100), List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x"))), REF("nil")))
                    )
                  ))
                ),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(FUNCTION_CALL(
                    Native(1100),
                    List(
                      FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                      FUNCTION_CALL(Native(1100), List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x"))), REF("nil")))
                    )
                  ))
                )
              )
            )
          )
        )),
      Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
    )
    Decompiler(contract: Contract, decompilerContext) shouldEq
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
         |        then WriteSet(cons(DataEntry("b", 1), cons(DataEntry("sender", x), nil)))
         |        else WriteSet(cons(DataEntry("a", a), cons(DataEntry("sender", x), nil)))
         |    }
         |
         |
         |@Verifier(t)
         |func verify () = true
         |""".stripMargin
  }

  property("Invoke contract decompilation") {
    val contract = Contract(
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
        )),
      None
    )
    Decompiler(contract, decompilerContext) shouldEq
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
    val expr = Terms.BLOCK(Terms.LET("param", CONST_BYTESTR(ByteStr(test.getBytes()))), REF("param"))
    Decompiler(expr, decompilerContext) shouldEq
      """let param = base58'3K3F4C'
        |param""".stripMargin
  }

  property("getter") {
    val expr = Terms.GETTER(Terms.FUNCTION_CALL(
                              function = FunctionHeader.User("testfunc"),
                              args = List(TRUE)
                            ),
                            "testfield")
    Decompiler(expr, decompilerContext) shouldEq
      """testfunc(true).testfield""".stripMargin
  }

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, decompilerContext) shouldEq
      """if (true)
        |    then 1
        |    else "XXX"""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")))
    Decompiler(expr, decompilerContext) shouldEq
      """if (true)
        |    then 1
        |    else if (true)
        |        then 1
        |        else "XXX"""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE, IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")), CONST_LONG(1))
    Decompiler(expr, decompilerContext) shouldEq
      """if (true)
        |    then if (true)
        |        then 1
        |        else "XXX"
        |    else 1""".stripMargin
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
                FUNCTION_CALL(Native(1), List(REF("$match0"), CONST_STRING("ExchangeTransaction"))),
                BLOCK(
                  LET("e", REF("$match0")),
                  BLOCK(
                    LET("days",
                        FUNCTION_CALL(Native(105), List(FUNCTION_CALL(Native(101), List(REF("height"), REF("startHeight"))), REF("interval")))),
                    IF(
                      IF(
                        IF(
                          FUNCTION_CALL(
                            Native(103),
                            List(
                              GETTER(REF("e"), "price"),
                              FUNCTION_CALL(Native(104),
                                            List(REF("startPrice"),
                                                 FUNCTION_CALL(Native(100),
                                                               List(CONST_LONG(1), FUNCTION_CALL(Native(104), List(REF("days"), REF("days")))))))
                            )
                          ),
                          FUNCTION_CALL(User("!"),
                                        List(FUNCTION_CALL(User("isDefined"),
                                                           List(GETTER(GETTER(GETTER(REF("e"), "sellOrder"), "assetPair"), "priceAsset"))))),
                          FALSE
                        ),
                        FUNCTION_CALL(
                          Native(103),
                          List(REF("exp"),
                               FUNCTION_CALL(Native(101),
                                             List(GETTER(GETTER(REF("e"), "sellOrder"), "expiration"),
                                                  GETTER(GETTER(REF("e"), "sellOrder"), "timestamp"))))
                        ),
                        FALSE
                      ),
                      FUNCTION_CALL(
                        Native(103),
                        List(REF("exp"),
                             FUNCTION_CALL(Native(101),
                                           List(GETTER(GETTER(REF("e"), "buyOrder"), "expiration"),
                                                GETTER(GETTER(REF("e"), "buyOrder"), "timestamp"))))
                      ),
                      FALSE
                    )
                  )
                ),
                IF(FUNCTION_CALL(Native(1), List(REF("$match0"), CONST_STRING("BurnTransaction"))), BLOCK(LET("tx", REF("$match0")), TRUE), FALSE)
              )
            )
          )
        )
      )
    )
    Decompiler(expr, decompilerContext) shouldEq
      """let startHeight = 1375557
        |let startPrice = 100000
        |let interval = (24 * 60)
        |let exp = ((100 * 60) * 1000)
        |let $match0 = tx
        |if (_isInstanceOf($match0, "ExchangeTransaction"))
        |    then {
        |        let e = $match0
        |        let days = ((height - startHeight) / interval)
        |        if (if (if ((e.price >= (startPrice * (1 + (days * days)))))
        |            then !(isDefined(e.sellOrder.assetPair.priceAsset))
        |            else false)
        |            then (exp >= (e.sellOrder.expiration - e.sellOrder.timestamp))
        |            else false)
        |            then (exp >= (e.buyOrder.expiration - e.buyOrder.timestamp))
        |            else false
        |        }
        |    else if (_isInstanceOf($match0, "BurnTransaction"))
        |        then {
        |            let tx = $match0
        |            true
        |            }
        |        else false""".stripMargin
  }

}
