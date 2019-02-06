package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.{FunctionHeader, compiler}
import com.wavesplatform.lang.{Common, StdLibVersion}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  val opcodes =
    scala.collection.immutable.Map[Short, String](1.toShort -> "+", 2.toShort -> "throw", 100.toShort -> "sigVerify", 1101.toShort -> "List")

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

    val ctx             = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val compledContract = compiler.ContractCompiler(ctx, parsedScript)

    compledContract.getOrElse("error").toString shouldBe
      "Contract(List(),List(ContractFunction(CallableAnnotation(i),FUNC(testfunc,List(amount),BLOCK(LET(pmt,CONST_LONG(1)),IF(FALSE,FUNCTION_CALL(Native(2),List(CONST_STRING(impossible))),FUNCTION_CALL(User(ContractResult),List(FUNCTION_CALL(User(WriteSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(DataEntry),List(CONST_STRING(1), CONST_STRING(1))))))), FUNCTION_CALL(User(TransferSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(ContractTransfer),List(GETTER(REF(i),caller), REF(amount), REF(unit)))))))))))))),None)"
  }

  property("Invoke contract with verifier decompilation") {
    val scriptText =
      """
        |
        |func fooHelper2() = {
        |   false
        |}
        |
        |func fooHelper() = {
        |   fooHelper2() || false
        |}
        |
        |@Callable(invocation)
        |func foo(a:ByteStr) = {
        |  let x = invocation.caller.bytes
        |  if (fooHelper())
        |    then WriteSet(List(DataEntry("b", 1), DataEntry("sender", x)))
        |    else WriteSet(List(DataEntry("a", a), DataEntry("sender", x)))
        |}
        |
        |@Verifier(t)
        |func verify() = {
        |  true
        |}
        |
      """.stripMargin
    val contract = Contract(
      List(FUNC("fooHelper2", List(), FALSE), FUNC("fooHelper", List(), IF(FUNCTION_CALL(User("fooHelper2"), List()), TRUE, FALSE))),
      List(
        CallableFunction(
          CallableAnnotation("invocation"),
          FUNC(
            "foo",
            List("a"),
            BLOCK(
              LET("x", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              IF(
                FUNCTION_CALL(User("fooHelper"), List()),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(FUNCTION_CALL(
                    Native(1102),
                    List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("b"), CONST_LONG(1))),
                         FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x"))))
                  ))
                ),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(FUNCTION_CALL(
                    Native(1102),
                    List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                         FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x"))))
                  ))
                )
              )
            )
          )
        )),
      Some(VerifierFunction(VerifierAnnotation("t"), FUNC("verify", List(), TRUE)))
    )
    Decompiler(contract: Contract, opcodes) shouldBe
      """func fooHelper2 () = {
        |    false
        |}
        |
        |func fooHelper () = {
        |    { if (
        |    fooHelper2()
        |    )
        |then
        |    true
        |else
        |    false
        |}
        |}
        |@Callable(invocation)
        |func foo (a) = {
        |    {
        |    let x =
        |                        invocation.caller.bytes;
        |    { if (
        |        fooHelper()
        |        )
        |    then
        |        WriteSet(<Native_1102>(DataEntry("b",1),DataEntry("sender",x)))
        |    else
        |        WriteSet(<Native_1102>(DataEntry("a",a),DataEntry("sender",x)))
        |    }
        |}
        |}@Verifier(t)
        |func verify () = {
        |    true
        |}""".stripMargin
  }

  property("Invoke contract decompilation") {
    val contract = Contract(
      List(),
      List(
        CallableFunction(
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
                    FUNCTION_CALL(
                      User("WriteSet"),
                      List(FUNCTION_CALL(Native(1101), List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("1"), CONST_STRING("1"))))))),
                    FUNCTION_CALL(
                      User("TransferSet"),
                      List(FUNCTION_CALL(Native(1101),
                                         List(FUNCTION_CALL(User("ContractTransfer"), List(GETTER(REF("i"), "caller"), REF("amount"), REF("unit"))))))
                    )
                  )
                )
              )
            )
          )
        )),
      None
    )

    Decompiler(contract: Contract, opcodes) shouldBe
      """
        |@Callable(i)
        |func testfunc (amount) = {
        |    {
        |    let pmt =
        |        1;
        |    { if (
        |        false
        |        )
        |    then
        |        throw("impossible")
        |    else
        |        ContractResult(WriteSet(List(DataEntry("1","1"))),TransferSet(List(ContractTransfer(i.caller,amount,unit))))
        |    }
        |}
        |}""".stripMargin
  }

  property("bytestring") {
    val test = Base58.encode("abc".getBytes("UTF-8"))
    // ([REVIEW]: may be i`am make a mistake here)
    val expr = Terms.BLOCK(Terms.LET("param", CONST_BYTESTR(ByteStr(test.getBytes()))), REF("param"))
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let param =
        |        base58'3K3F4C';
        |    param
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
