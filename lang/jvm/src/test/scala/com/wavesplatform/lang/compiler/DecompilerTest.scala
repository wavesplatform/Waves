package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract._
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.BinaryOperation
import com.wavesplatform.lang.v1.{CTX, FunctionHeader, compiler}
import com.wavesplatform.lang.{Common, Global, StdLibVersion}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  val CTX: CTX =
    Monoid.combineAll(Seq(PureContext.build(com.wavesplatform.lang.StdLibVersion.V3), CryptoContext.build(Global)))

  val decompilerContext = CTX.decompilerContext

  property("ctx debug test") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val defs = ctx.functionDefs.filterKeys(BinaryOperation.opsByPriority.flatten.map(x => BinaryOperation.opsToFunctions(x) -> x).toMap.keys.toList.contains(_))
      .mapValues(_.map(_.header)
        .filter(_.isInstanceOf[Native])
        .map(_.asInstanceOf[Native].name)
      )
      .toList
      .flatMap { case (name, codes) => codes.map((_, name)) }
    defs.mkString("\n" ).toString shouldBe
      """(104,*)
        |(106,%)
        |(103,>=)
        |(101,-)
        |(0,==)
        |(100,+)
        |(300,+)
        |(203,+)
        |(105,/)
        |(102,>)""".stripMargin
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr, decompilerContext) shouldBe "{ let a = 1; true }"
  }

  property("native function call with one arg") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(500),
      args = List(TRUE)
    )
    Decompiler(expr, decompilerContext) shouldBe "sigVerify(true)"
  }

  property("native function call with two arg (binary operations)") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContext) shouldBe "1 + 2"
  }

  property("unknown native function call") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(254),
      args = List(CONST_LONG(1), CONST_LONG(2))
    )
    Decompiler(expr, decompilerContext) shouldBe "Decompile Error: Wrong opcode: <254> with args:(1,2)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, decompilerContext) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, decompilerContext) shouldBe "foo()"
  }

  property("definition of user function") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr, decompilerContext) shouldBe
      """func foo (bar,buz) = {
        |    true
        |}
        |""".stripMargin
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    let vari =
        |        p;
        |    true
        |}""".stripMargin
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(Terms.LET("v", CONST_LONG(1)),
        Terms.FUNCTION_CALL(
          function = FunctionHeader.Native(100),
          args = List(REF("v"), CONST_LONG(2))
        ))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    let v =
        |        1;
        |    v + 2
        |}""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("p",
        Terms.BLOCK(Terms.LET("v", CONST_LONG(1)),
          Terms.FUNCTION_CALL(
            function = FunctionHeader.Native(100),
            args = List(REF("v"), CONST_LONG(2))))),
      Terms.FUNCTION_CALL(
        function = FunctionHeader.Native(100),
        args = List(REF("p"), CONST_LONG(3))))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    let p =
        |        {
        |            let v =
        |                1;
        |            v + 2
        |        };
        |    p + 3
        |}""".stripMargin
  }

  property("old match") {
    val expr = Terms.BLOCK(
      LET("v", CONST_LONG(1)),
      IF(
        IF(
          FUNCTION_CALL(
            function = FunctionHeader.Native(100),
            args = List(REF("v"), CONST_LONG(2))),
          TRUE,
          FUNCTION_CALL(
            function = FunctionHeader.Native(100),
            args = List(REF("v"), CONST_LONG(3)))),
        BLOCK(LET("p", REF("v")), TRUE),
        FALSE))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    let v =
        |        1;
        |    {
        |        if (
        |            {
        |                if (
        |                    v + 2
        |                )
        |                then
        |                    true
        |                else
        |                    v + 3
        |            }
        |        )
        |        then
        |            {
        |                let p =
        |                    v;
        |                true
        |            }
        |        else
        |            false
        |    }
        |}""".stripMargin
  }

  property("new match") {
    val expr = Terms.BLOCK(
      Terms.LET("v", CONST_LONG(1)),
      Terms.IF(
        Terms.IF(
          Terms.FUNCTION_CALL(
            function = FunctionHeader.Native(100),
            args = List(REF("v"), CONST_LONG(2))),
          TRUE,
          Terms.FUNCTION_CALL(
            function = FunctionHeader.Native(100),
            args = List(REF("v"), CONST_LONG(3)))),
        Terms.BLOCK(Terms.LET("z", CONST_LONG(4)), TRUE),
        FALSE
      )
    )
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    let v =
        |        1;
        |    {
        |        if (
        |            {
        |                if (
        |                    v + 2
        |                )
        |                then
        |                    true
        |                else
        |                    v + 3
        |            }
        |        )
        |        then
        |            {
        |                let z =
        |                    4;
        |                true
        |            }
        |        else
        |            false
        |    }
        |}""".stripMargin
  }

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
      """Contract(List(),List(CallableFunction(CallableAnnotation(i),FUNC(testfunc,List(amount),BLOCK(LET(pmt,CONST_LONG(1)),IF(FALSE,FUNCTION_CALL(Native(2),List(CONST_STRING(impossible))),FUNCTION_CALL(User(ContractResult),List(FUNCTION_CALL(User(WriteSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(DataEntry),List(CONST_STRING(1), CONST_STRING(1))))))), FUNCTION_CALL(User(TransferSet),List(FUNCTION_CALL(Native(1101),List(FUNCTION_CALL(User(ContractTransfer),List(GETTER(REF(i),caller), REF(amount), REF(unit)))))))))))))),None)"""

  }

  property("Invoke contract with verifier decompilation") {
    val contract = Contract(
      List(
        FUNC("foo", List(), FALSE),
        FUNC("bar", List(),
          IF(
            FUNCTION_CALL(User("foo"),
              List()), TRUE, FALSE))),
      List(
        CallableFunction(
          CallableAnnotation("invocation"),
          FUNC("baz", List("a"), BLOCK(
              LET("x", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              IF(
                FUNCTION_CALL(User("foo"), List()),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(FUNCTION_CALL(
                    Native(1102),
                    List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("b"), CONST_LONG(1))),
                         FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x"))))))),
                FUNCTION_CALL(
                  User("WriteSet"),
                  List(FUNCTION_CALL(
                    Native(1102),
                    List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                         FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("x")))))))))))),
      Some(VerifierFunction(VerifierAnnotation("t"),
        FUNC("verify", List(), TRUE))))
    Decompiler(contract: Contract, decompilerContext) shouldBe
      """func foo () = {
        |    false
        |}
        |
        |
        |func bar () = {
        |    {
        |        if (
        |            foo()
        |        )
        |        then
        |            true
        |        else
        |            false
        |    }
        |}
        |
        |@Callable(invocation)
        |func baz (a) = {
        |    {
        |        let x =
        |            invocation.caller.bytes;
        |        {
        |            if (
        |                foo()
        |            )
        |            then
        |                WriteSet(List(DataEntry("b",1),DataEntry("sender",x)))
        |            else
        |                WriteSet(List(DataEntry("a",a),DataEntry("sender",x)))
        |        }
        |    }
        |}
        |
        |@Verifier(t)
        |func verify () = {
        |    true
        |}
        |""".stripMargin
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
                        List(FUNCTION_CALL(User("ContractTransfer"),
                          List(GETTER(REF("i"), "caller"),
                            REF("amount"),
                            REF("unit")))))))))))))),
      None)
    Decompiler(contract: Contract, decompilerContext) shouldBe
      """
        |@Callable(i)
        |func testfunc (amount) = {
        |    {
        |        let pmt =
        |            1;
        |        {
        |            if (
        |                false
        |            )
        |            then
        |                throw("impossible")
        |            else
        |                ContractResult(WriteSet(List(DataEntry("1","1"))),TransferSet(List(ContractTransfer(i.caller,amount,unit))))
        |        }
        |    }
        |}
        |""".stripMargin
  }

  property("bytestring") {
    val test = Base58.encode("abc".getBytes("UTF-8"))
    // ([REVIEW]: may be i`am make a mistake here)
    val expr = Terms.BLOCK(Terms.LET("param", CONST_BYTESTR(ByteStr(test.getBytes()))), REF("param"))
    Decompiler(expr, decompilerContext) shouldBe
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
    Decompiler(expr, decompilerContext) shouldBe
      """testfunc(true).testfield""".stripMargin
  }

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    if (
        |        true
        |    )
        |    then
        |        1
        |    else
        |        "XXX"
        |}""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1), IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    if (
        |        true
        |    )
        |    then
        |        1
        |    else
        |        {
        |            if (
        |                true
        |            )
        |            then
        |                1
        |            else
        |                "XXX"
        |        }
        |}""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE, IF(TRUE, CONST_LONG(1), CONST_STRING("XXX")), CONST_LONG(1))
    Decompiler(expr, decompilerContext) shouldBe
      """{
        |    if (
        |        true
        |    )
        |    then
        |        {
        |            if (
        |                true
        |            )
        |            then
        |                1
        |            else
        |                "XXX"
        |        }
        |    else
        |        1
        |}""".stripMargin
  }

}
