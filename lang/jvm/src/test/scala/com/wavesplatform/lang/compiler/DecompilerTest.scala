package com.wavesplatform.lang.compiler

import java.util.concurrent.Callable

import cats.instances.unit
import cats.kernel.Monoid
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.{Common, Version}
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ContractResult
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.BinaryOperation.{LT_OP, SUB_OP, SUM_OP}
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.{ANNOTATEDFUNC, ANNOTATION, BINARY_OP, CONTRACT, MATCH, MATCH_CASE}
import com.wavesplatform.lang.v1.parser.Expressions.Pos.{AnyPos, RealPos}
import fastparse.parsers.Combinators.Not
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable.ArrayBuffer

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  val opcodes = scala.collection.immutable.Map[Short, String](1.toShort -> "+", 100.toShort -> "sigVerify")

  property("Invoke contract compilation") {
    val scriptText =
      """
        |	@Callable(i)
        |	func testfunc(amount: Int) = {
        |   let pmt = 1
        |
        |   if (false)
        |   then
        |     throw("impossible")
        |   else {
        |	  	ContractResult(
        |        WriteSet(List(DataEntry("1", "1"))),
        |        TransferSet(List(ContractTransfer(i.caller, amount, unit)))
        |     )
        |   }
        |	}
      """.stripMargin
    val parsedScript = com.wavesplatform.lang.v1.parser.Parser.parseContract(scriptText).get.value

    val ctx             = Monoid.combine(compilerContext, WavesContext.build(Version.ContractV, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val compledContract = compiler.ContractCompiler(ctx, parsedScript)

    compledContract.getOrElse("error").toString shouldBe
      "Contract(List(),List(ContractFunction(CallableAnnotation(i),FUNC(testfunc,List(amount),BLOCK(LET(pmt,CONST_LONG(1)),IF(FALSE,FUNCTION_CALL(Native(2),List(CONST_STRING(impossible))),FUNCTION_CALL(User(ContractResult),List(FUNCTION_CALL(User(WriteSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(DataEntry),List(CONST_STRING(1), CONST_STRING(1))))))), FUNCTION_CALL(User(TransferSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(ContractTransfer),List(GETTER(REF(i),caller), REF(amount), REF(unit)))))))))))))),None)"
  }

  property("Invoke contract decompilation") {
    val contract = Contract(
      List(), // dec
      List(   // cfs
        ContractFunction(
          CallableAnnotation("i"),
          FUNC(
            "testfunc",
            List("amount"),
            BLOCK(
              LET("pmt", CONST_LONG(1)),
              IF(
                FALSE,
                FUNCTION_CALL(Native(2), List(CONST_STRING("impossible"))),
                FUNCTION_CALL(
                  User("ContractResult"),
                  List(
                    FUNCTION_CALL(User("WriteSet"),
                                  List(FUNCTION_CALL(Native(1101), List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("1"), CONST_STRING("1"))))))),
                    FUNCTION_CALL(User("TransferSet"),
                                  List(
                                    FUNCTION_CALL(Native(1101),
                                                  List(FUNCTION_CALL(User("ContractTransfer"), List(GETTER(REF("i"), "caller"), REF("amount"), REF("unit")))))))))))))),
      None)

    Decompiler(contract :Contract, 0, opcodes) shouldBe
      """(d@Callable(i)
        |func testfunc (amount) = {
        |    {
        |    let pmt =
        |        1;
        |    { if (
        |        false
        |        )
        |    then
        |        <Native_2>("impossible")
        |    else
        |        ContractResult(WriteSet(<Native_1101>(DataEntry("1","1"))),TransferSet(<Native_1101>(ContractTransfer(i.caller,amount,unit))))
        |    }
        |}
        |}""".stripMargin
  }


  property("decompilation of real contract from dKiselev") {
    val scriptText =
      Terms.BLOCK(
        Terms.LET("startHeight", CONST_LONG(1375557)),
        Terms.BLOCK(
          Terms.LET("startPrice", CONST_LONG(100000)),
          Terms.BLOCK(
            Terms.LET("interval", Terms.FUNCTION_CALL(FunctionHeader.Native(104), List(CONST_LONG(24), CONST_LONG(60)))),
            Terms.BLOCK(
              Terms.LET(
                "exp",
                Terms.FUNCTION_CALL(FunctionHeader.Native(104),
                                    List(Terms.FUNCTION_CALL(FunctionHeader.Native(104), List(CONST_LONG(100), CONST_LONG(60))), CONST_LONG(1000)))
              ),
              Terms.BLOCK(
                Terms.LET("$match0", REF("tx")),
                IF(
                  Terms.FUNCTION_CALL(FunctionHeader.Native(1), List(REF("$match0"), CONST_STRING("ExchangeTransaction"))),
                  Terms.BLOCK(
                    Terms.LET("e", REF("$match0")),
                    Terms.BLOCK(
                      Terms.LET(
                        "days",
                        Terms.FUNCTION_CALL(FunctionHeader.Native(105),
                                            List(Terms.FUNCTION_CALL(FunctionHeader.Native(101), List(REF("height"), REF("startHeight"))),
                                                 REF("interval")))
                      ),
                      IF(
                        IF(
                          IF(
                            Terms.FUNCTION_CALL(
                              FunctionHeader.Native(103),
                              List(
                                GETTER(REF("e"), "price"),
                                Terms.FUNCTION_CALL(
                                  FunctionHeader.Native(104),
                                  List(
                                    REF("startPrice"),
                                    Terms.FUNCTION_CALL(FunctionHeader.Native(100),
                                                        List(CONST_LONG(1),
                                                             Terms.FUNCTION_CALL(FunctionHeader.Native(104), List(REF("days"), REF("days")))))
                                  )
                                )
                              )
                            ),
                            Terms.FUNCTION_CALL(
                              FunctionHeader.User("!"),
                              List(Terms.FUNCTION_CALL(FunctionHeader.User("isDefined"),
                                                       List(GETTER(GETTER(GETTER(REF("e"), "sellOrder"), "assetPair"), "priceAsset"))))
                            ),
                            FALSE
                          ),
                          Terms.FUNCTION_CALL(
                            FunctionHeader.Native(103),
                            List(
                              REF("exp"),
                              Terms.FUNCTION_CALL(FunctionHeader.Native(101),
                                                  List(GETTER(GETTER(REF("e"), "sellOrder"), "expiration"),
                                                       GETTER(GETTER(REF("e"), "sellOrder"), "timestamp")))
                            )
                          ),
                          FALSE
                        ),
                        Terms.FUNCTION_CALL(
                          FunctionHeader.Native(103),
                          List(
                            REF("exp"),
                            Terms.FUNCTION_CALL(FunctionHeader.Native(101),
                                                List(GETTER(GETTER(REF("e"), "buyOrder"), "expiration"),
                                                     GETTER(GETTER(REF("e"), "buyOrder"), "timestamp")))
                          )
                        ),
                        FALSE
                      )
                    )
                  ),
                  IF(
                    Terms.FUNCTION_CALL(FunctionHeader.Native(1), List(REF("$match0"), CONST_STRING("BurnTransaction"))),
                    Terms.BLOCK(Terms.LET("tx", REF("$match0")), TRUE),
                    FALSE
                  )
                )
              )
            )
          )
        )
      )
    Decompiler(scriptText, opcodes) shouldBe
      """{
          |    let startHeight =
          |        1375557;
          |    {
          |        let startPrice =
          |            100000;
          |        {
          |            let interval =
          |                <Native_104>(24,60);
          |            {
          |                let exp =
          |                    <Native_104>(<Native_104>(100,60),1000);
          |                {
          |                    let $match0 =
          |                        tx;
          |                    { if (
          |                        +($match0,"ExchangeTransaction")
          |                        )
          |                    then
          |                        {
          |                            let e =
          |                                $match0;
          |                            {
          |                                let days =
          |                                    <Native_105>(<Native_101>(height,startHeight),interval);
          |                                { if (
          |                                    { if (
          |                                        { if (
          |                                            <Native_103>(e.price,<Native_104>(startPrice,sigVerify(1,<Native_104>(days,days))))
          |                                            )
          |                                        then
          |                                            !(isDefined(e.sellOrder.assetPair.priceAsset))
          |                                        else
          |                                            false
          |                                        }
          |                                        )
          |                                    then
          |                                        <Native_103>(exp,<Native_101>(e.sellOrder.expiration,e.sellOrder.timestamp))
          |                                    else
          |                                        false
          |                                    }
          |                                    )
          |                                then
          |                                    <Native_103>(exp,<Native_101>(e.buyOrder.expiration,e.buyOrder.timestamp))
          |                                else
          |                                    false
          |                                }
          |                            }
          |                        }
          |                    else
          |                        { if (
          |                            +($match0,"BurnTransaction")
          |                            )
          |                        then
          |                            {
          |                                let tx =
          |                                    $match0;
          |                                true
          |                            }
          |                        else
          |                            false
          |                        }
          |                    }
          |                }
          |            }
          |        }
          |    }
          |}""".stripMargin
  }

  property("decompilation of real contract from Maxim Smolyakov") {
    val scriptText = BLOCK(
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
                  LET("days", FUNCTION_CALL(Native(105), List(FUNCTION_CALL(Native(101), List(REF("height"), REF("startHeight"))), REF("interval")))),
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
                                                             List(CONST_LONG(1), FUNCTION_CALL(Native(104), List(REF("days"), REF("days"))))))))),
                        FUNCTION_CALL(
                          User("!"),
                          List(FUNCTION_CALL(User("isDefined"), List(GETTER(GETTER(GETTER(REF("e"), "sellOrder"), "assetPair"), "priceAsset"))))),
                        FALSE),
                      FUNCTION_CALL(
                        Native(103),
                        List(REF("exp"),
                             FUNCTION_CALL(Native(101),
                                           List(GETTER(GETTER(REF("e"), "sellOrder"), "expiration"),
                                                GETTER(GETTER(REF("e"), "sellOrder"), "timestamp"))))),
                      FALSE),
                    FUNCTION_CALL(
                      Native(103),
                      List(REF("exp"),
                           FUNCTION_CALL(Native(101),
                                         List(GETTER(GETTER(REF("e"), "buyOrder"), "expiration"), GETTER(GETTER(REF("e"), "buyOrder"), "timestamp"))))),
                    FALSE))),
              IF(FUNCTION_CALL(Native(1), List(REF("$match0"), CONST_STRING("BurnTransaction"))), BLOCK(LET("tx", REF("$match0")), TRUE), FALSE)))))))
    Decompiler(scriptText, opcodes) shouldBe
      """{
        |    let startHeight =
        |        1375557;
        |    {
        |        let startPrice =
        |            100000;
        |        {
        |            let interval =
        |                <Native_104>(24,60);
        |            {
        |                let exp =
        |                    <Native_104>(<Native_104>(100,60),1000);
        |                {
        |                    let $match0 =
        |                        tx;
        |                    { if (
        |                        +($match0,"ExchangeTransaction")
        |                        )
        |                    then
        |                        {
        |                            let e =
        |                                $match0;
        |                            {
        |                                let days =
        |                                    <Native_105>(<Native_101>(height,startHeight),interval);
        |                                { if (
        |                                    { if (
        |                                        { if (
        |                                            <Native_103>(e.price,<Native_104>(startPrice,sigVerify(1,<Native_104>(days,days))))
        |                                            )
        |                                        then
        |                                            !(isDefined(e.sellOrder.assetPair.priceAsset))
        |                                        else
        |                                            false
        |                                        }
        |                                        )
        |                                    then
        |                                        <Native_103>(exp,<Native_101>(e.sellOrder.expiration,e.sellOrder.timestamp))
        |                                    else
        |                                        false
        |                                    }
        |                                    )
        |                                then
        |                                    <Native_103>(exp,<Native_101>(e.buyOrder.expiration,e.buyOrder.timestamp))
        |                                else
        |                                    false
        |                                }
        |                            }
        |                        }
        |                    else
        |                        { if (
        |                            +($match0,"BurnTransaction")
        |                            )
        |                        then
        |                            {
        |                                let tx =
        |                                    $match0;
        |                                true
        |                            }
        |                        else
        |                            false
        |                        }
        |                    }
        |                }
        |            }
        |        }
        |    }
        |}""".stripMargin
  }

  property("getter") {
    val expr = Terms.GETTER(Terms.FUNCTION_CALL(
                              function = FunctionHeader.User("testfunc"),
                              args = List(TRUE)
                            ),
                            "testfield")
    Decompiler(expr, opcodes) shouldBe
      """testfunc(true).testfield""".stripMargin
  }

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    "XXX"
        |}""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |}""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE, IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")), CONST_LONG(1))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |else
        |    1
        |}""".stripMargin
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr, opcodes) shouldBe "{ let a = 1; true }"
  }

  property("native function call with one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "sigVerify(true)"
  }

  property("undefined native function call  one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(101),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "<Native_101>(true)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, opcodes) shouldBe "foo()"
  }

  property("definition of user function") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr, opcodes) shouldBe
      """func foo (bar,buz) = {
         |    true
         |}""".stripMargin
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr, opcodes) shouldBe
      """{
      |    let vari =
      |        p;
      |    true
      |}""".stripMargin
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(Terms.LET("v", REF("p")),
                           Terms.FUNCTION_CALL(
                             PureContext._isInstanceOf.header,
                             List(REF("v"), Terms.CONST_STRING("a"))
                           ))
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    +(v,"a")
        |}""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("v",
                Terms.BLOCK(Terms.LET("v", REF("p")),
                            Terms.FUNCTION_CALL(
                              PureContext._isInstanceOf.header,
                              List(REF("v"), Terms.CONST_STRING("a"))
                            ))),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        {
        |            let v =
        |                p;
        |            +(v,"a")
        |        };
        |    +(v,"a")
        |}""".stripMargin
  }

  property("old match") {
    val expr = Terms.BLOCK(
      LET("v", REF("p")),
      IF(
        IF(
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("a"))
          ),
          TRUE,
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("b"))
          )
        ),
        BLOCK(LET("p", REF("v")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    { if (
        |        { if (
        |            +(v,"a")
        |            )
        |        then
        |            true
        |        else
        |            +(v,"b")
        |        }
        |        )
        |    then
        |        {
        |            let p =
        |                v;
        |            true
        |        }
        |    else
        |        false
        |    }
        |}""".stripMargin
  }

  property("new match") {
    val expr = Terms.BLOCK(
      Terms.LET("v", REF("p")),
      Terms.IF(
        Terms.IF(
          Terms.FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), Terms.CONST_STRING("a"))
          ),
          TRUE,
          Terms.FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), Terms.CONST_STRING("b"))
          )
        ),
        Terms.BLOCK(Terms.LET("z", Terms.REF("x")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
      |    let v =
      |        p;
      |    { if (
      |        { if (
      |            +(v,"a")
      |            )
      |        then
      |            true
      |        else
      |            +(v,"b")
      |        }
      |        )
      |    then
      |        {
      |            let z =
      |                x;
      |            true
      |        }
      |    else
      |        false
      |    }
      |}""".stripMargin
  }

}
