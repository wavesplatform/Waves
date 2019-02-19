package com.wavesplatform.lang.compiler
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.contract.Contract
import com.wavesplatform.lang.contract.Contract.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, StdLibVersion}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("contract compiles when uses annotation bindings and correct return type") {
    val ctx = Monoid.combine(compilerContext,
                             WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), isTokenContext = false).compilerContext)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender0)))
          | }
          |
          | @Verifier(t)
          | func verify() = {
          |   t.id == base58''
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val expectedResult = Right(
      Contract(
        List.empty,
        List(CallableFunction(
          CallableAnnotation("invocation"),
          Terms.FUNC(
            "foo",
            List("a"),
            LET_BLOCK(
              LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
              FUNCTION_CALL(
                User(FieldNames.WriteSet),
                List(FUNCTION_CALL(
                  Native(1102),
                  List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                       FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("sender0"))))
                ))
              )
            )
          )
        )),
        Some(
          VerifierFunction(
            VerifierAnnotation("t"),
            FUNC("verify", List.empty, FUNCTION_CALL(Native(FunctionIds.EQ), List(GETTER(REF("t"), "id"), CONST_BYTESTR(ByteStr.empty))))
          ))
      ))
    compiler.ContractCompiler(ctx, expr) shouldBe expectedResult
  }

  property("contract compiles callable functions independently") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender0)))
          | }
          |
          | @Callable(invocation)
          | func foo1(a:ByteStr) = {
          |  foo(a)
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("Can't find a function 'foo'")
  }

  property("contract can access declarations") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          | let x = 0
          |
          | func bar() = {
          |   x
          | }
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  let aux = bar()
          |  let sender0 = invocation.caller.bytes
          |  WriteSet(List(DataEntry("a", a), DataEntry("sender", sender0)))
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract compiles fails when incorrect return type") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  a + invocation.caller.bytes
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce(FieldNames.Error)
  }

  property("contract compiles fails if has more than one verifier function") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          |
          | @Verifier(tx)
          | func foo() = {
          |  true
          | }
          | 
          | @Verifier(tx)
          | func bar() = {
          |  false
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("more than 1 verifier function")
  }

  property("contract compiles fails if has unknown annotation") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          | @Whooaaaa(arg)
          | func foo() = {
          |  true
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("Annotation not recognized")
  }

  property("verifier function must have 0 arguments") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          | @Verifier(arg)
          | func foo(a: Int) = {
          |  true
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("must have 0 arguments")
  }

  property("hodlContract") {
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(StdLibVersion.V3),
          CryptoContext.build(com.wavesplatform.lang.Global),
          WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false)
        ))
      .compilerContext
    val expr = {
      val script =
        """
          |
          |	@Callable(i)
          |	func deposit() = {
          |   let pmt = extract(i.payment)
          |   if (isDefined(pmt.asset)) then throw("can hodl waves only at the moment")
          |   else {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |	  	let newAmount = currentAmount + pmt.amount
          |	  	WriteSet(List(DataEntry(currentKey, newAmount)))
          |
          |   }
          |	}
          |
          | @Callable(i)
          | func withdraw(amount: Int) = {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |		let newAmount = currentAmount - amount
          |	 if (amount < 0)
          |			then throw("Can't withdraw negative amount")
          |  else if (newAmount < 0)
          |			then throw("Not enough balance")
          |			else ContractResult(
          |					WriteSet(List(DataEntry(currentKey, newAmount))),
          |					TransferSet(List(ContractTransfer(i.caller, amount, unit)))
          |				)
          |	}
          |
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract functions could return parent type values") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteStr) = {
          |  throw()
          | }
          |
          | @Callable(i)
          | func bar() = {
          |   if (true) then WriteSet(List(DataEntry("entr1","entr2")))
          |   else TransferSet(List(ContractTransfer(i.caller, wavesBalance(i.contractAddress), base58'somestr')))
          | }
          |
          | @Verifier(t)
          | func verify() = {
          |   throw()
          | }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract compilation fails if functions has the same name") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          |@Callable(i)
          |func sameName() = {
          |   WriteSet(List(DataEntry("a", "a")))
          |}
          |
          |@Callable(i)
          |func sameName() = {
          |   WriteSet(List(DataEntry("b", "b")))
          |}
          |
          |@Verifier(i)
          |func sameName() = {
          |   true
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("Contract functions must have unique names")
  }

  property("contract compilation fails if declaration and annotation bindings has the same name") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          |let x = 42
          |
          |@Callable(x)
          |func some(i: Int) = {
          |    WriteSet(List(DataEntry("a", "a")))
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("already defined")
  }

  property("contract compilation fails if annotation bindings and func args has the same name") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          |@Callable(i)
          |func some(i: Int) = {
          |   if (i.contractAddress == "abc") then
          |      WriteSet(List(DataEntry("a", "a")))
          |   else
          |      WriteSet(List(DataEntry("a", "b")))
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("override annotation bindings")
  }

  property("contract compiles if annotation bindings and another func args has the same name") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          |@Callable(x)
          |func foo(i: Int) = {
          |    WriteSet(List(DataEntry("a", "a")))
          |}
          |
          |@Callable(i)
          |func bar(x: Int) = {
          |    WriteSet(List(DataEntry("a", "a")))
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract compiles if declaration vars and func args has the same name") {
    val ctx = Monoid.combine(compilerContext, WavesContext.build(StdLibVersion.V3, Common.emptyBlockchainEnvironment(), false).compilerContext)
    val expr = {
      val script =
        """
          |
          |let x = 42
          |
          |@Callable(i)
          |func some(x: Int) = {
          |    WriteSet(List(DataEntry("a", "a")))
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }
}
