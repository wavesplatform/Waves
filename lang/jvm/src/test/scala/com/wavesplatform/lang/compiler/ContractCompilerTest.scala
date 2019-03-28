package com.wavesplatform.lang.compiler
import cats.kernel.Monoid
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.{NoShrink, produce}
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction, VerifierAnnotation, VerifierFunction}
import com.wavesplatform.lang.utils.DirectiveSet
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.{CompilerContext, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, ContentType, ScriptType, StdLibVersion}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractCompilerTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("contract compiles when uses annotation bindings and correct return type") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.DApp).explicitGet(),
          Common.emptyBlockchainEnvironment()
        )
        .compilerContext
    )
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteVector) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
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
      DApp(
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
                  Native(1100),
                  List(
                    FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a"), REF("a"))),
                    FUNCTION_CALL(Native(1100), List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), REF("sender0"))), REF("nil")))
                  )
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

  private val cmpCtx: CompilerContext =
    WavesContext
      .build(
        DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.DApp).explicitGet(),
        Common.emptyBlockchainEnvironment()
      )
      .compilerContext

  property("contract compiles callable functions independently") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteVector) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
          | }
          |
          | @Callable(invocation)
          | func foo1(a:ByteVector) = {
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
    val ctx = Monoid.combine(compilerContext, cmpCtx)
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
          | func foo(a:ByteVector) = {
          |  let aux = bar()
          |  let sender0 = invocation.caller.bytes
          |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
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
          | func foo(a:ByteVector) = {
          |  a + invocation.caller.bytes
          | }
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce(FieldNames.Error)
  }

  property("annotation binding can have the same name as annotated function") {
    val ctx = compilerContext
    val expr = {
      val script =
        """
          |
          |@Callable(sameName)
          |func sameName() = {
          |   throw()
          |}
          |
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
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
          WavesContext.build(
            DirectiveSet(StdLibVersion.V3, ScriptType.Account, ContentType.DApp).explicitGet(),
            Common.emptyBlockchainEnvironment()
          )
        ))
      .compilerContext
    val expr = {
      val script =
        """
          |
          |	@Callable(i)
          |	func deposit() = {
          |   let pmt = i.payment.value()
          |   if (isDefined(pmt.assetId)) then throw("can hodl waves only at the moment")
          |   else {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(this, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |	  	let newAmount = currentAmount + pmt.amount
          |	  	WriteSet([DataEntry(currentKey, newAmount)])
          |
          |   }
          |	}
          |
          | @Callable(i)
          | func withdraw(amount: Int) = {
          |	  	let currentKey = toBase58String(i.caller.bytes)
          |	  	let currentAmount = match getInteger(this, currentKey) {
          |	  		case a:Int => a
          |	  		case _ => 0
          |	  	}
          |		let newAmount = currentAmount - amount
          |	 if (amount < 0)
          |			then throw("Can't withdraw negative amount")
          |  else if (newAmount < 0)
          |			then throw("Not enough balance")
          |			else ScriptResult(
          |					WriteSet([DataEntry(currentKey, newAmount)]),
          |					TransferSet([ScriptTransfer(i.caller, amount, unit)])
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
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(a:ByteVector) = {
          |  throw()
          | }
          |
          | @Callable(i)
          | func bar() = {
          |   if (true) then WriteSet([DataEntry("entr1","entr2")])
          |   else TransferSet([ScriptTransfer(i.caller, wavesBalance(this), base58'somestr')])
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
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |@Callable(i)
          |func sameName() = {
          |   WriteSet([DataEntry("a", "a")])
          |}
          |
          |@Callable(i)
          |func sameName() = {
          |   WriteSet([DataEntry("b", "b")])
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
    compiler.ContractCompiler(ctx, expr) should produce("is already defined")
  }

  property("contract compilation fails if declaration and annotation bindings has the same name") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |let x = 42
          |
          |@Callable(x)
          |func some(i: Int) = {
          |    WriteSet([DataEntry("a", "a")])
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("already defined")
  }

  property("contract compilation fails if annotation bindings and func args has the same name") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |@Callable(i)
          |func some(i: Int) = {
          |   if (this == "abc") then
          |      WriteSet([DataEntry("a", "a")])
          |   else
          |      WriteSet([DataEntry("a", "b")])
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce("override annotation bindings")
  }

  property("contract compiles if annotation bindings and another func args has the same name") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |@Callable(x)
          |func foo(i: Int) = {
          |    WriteSet([DataEntry("a", "a")])
          |}
          |
          |@Callable(i)
          |func bar(x: Int) = {
          |    WriteSet([DataEntry("a", "a")])
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract compiles if declaration vars and func args has the same name") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |let x = 42
          |
          |@Callable(i)
          |func some(x: Int) = {
          |    WriteSet([DataEntry("a", "a")])
          |}
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("contract compiles if it use invoke script fields: payment, feeAssetId") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |  {-# STDLIB_VERSION 3 #-}
          |  {-# CONTENT_TYPE DAPP #-}
          |  let a = 42
          |
          |  @Verifier(tx)
          |  func verify() = {
          |    match tx {
          |      case tx:InvokeScriptTransaction =>
          |        isDefined(tx.payment) && isDefined(tx.feeAssetId)
          |      case _ => true
          |    }
          |  }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) shouldBe 'right
  }

  property("matching case with non-existing type should produce error message with suitable types") {
    val ctx = Monoid.combine(compilerContext, cmpCtx)
    val expr = {
      val script =
        """
          |
          |  @Verifier(tx)
          |  func test() =
          |    match tx {
          |      case _: UndefinedType => true
          |      case _                => false
          |    }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val verifierTypes = WavesContext.verifierInput.types.map(_.name)
    compiler.ContractCompiler(ctx, expr) should produce(verifierTypes.toString)
  }

  property("expression matching case with non-existing type should produce error message with suitable types") {
    val ctx           = Monoid.combine(compilerContext, cmpCtx)
    val verifierTypes = WavesContext.verifierInput.types.map(_.name)

    val expr = {
      val script =
        s"""
          |
          |  func local(tx: ${verifierTypes.mkString("|")}) = tx
          |
          |  @Verifier(tx)
          |  func test() =
          |    match local(tx) {
          |      case _: UndefinedType => true
          |      case _                => false
          |  }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce(verifierTypes.toString)
  }

  ignore("matching case with union type containing non-existing type should produce error message with suitable types") {
    val ctx           = Monoid.combine(compilerContext, cmpCtx)
    val verifierTypes = WavesContext.verifierInput.types.map(_.name)

    val expr = {
      val script =
        s"""
           |
           |  @Verifier(tx)
           |  func test() =
           |    match tx {
           |      case _: ${verifierTypes.head} | UndefinedType => true
           |      case _                                        => false
           |    }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr) should produce(verifierTypes.toString)
  }
}
