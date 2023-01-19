package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import cats.syntax.semigroup.*
import com.google.protobuf.ByteString
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.*
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ScriptResultSource, Terms, TestCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, Types, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{ContractLimits, compiler}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.test.*
import org.scalatest.Assertion

class ContractCompilerTest extends PropSpec {
  private def dAppCtxForV(version: StdLibVersion) =
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, version).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(version, Account, DAppType).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
      .compilerContext

  private val dAppV3Ctx: CompilerContext = dAppCtxForV(V3)
  private val dAppV4Ctx: CompilerContext = dAppCtxForV(V4)

  property("contract compiles with comment in function body") {
    TestCompiler(V5).compile(
      """
        |func foo() = {
        |  #comment
        |  strict a = 1
        |  a
        |  #comment
        |}
      """.stripMargin
    ) shouldBe Right(
      DApp(
        DAppMeta(
          version = 2,
          List()
        ),
        List(
          FUNC(
            "foo",
            List(),
            /**/ LET_BLOCK(
              LET("a", CONST_LONG(1)),
              IF(
                FUNCTION_CALL(Native(0), List(REF("a"), REF("a"))),
                REF("a"),
                FUNCTION_CALL(Native(2), List(CONST_STRING("Strict value is not equal to itself.").explicitGet()))
              )
            )
          )
        ),
        List(),
        None
      )
    )
  }

  property("contract compiles when uses annotation bindings and correct return type") {
    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 1,
          List(
            CallableFuncSignature(ByteString.copyFrom(Array[Byte](2))),
            CallableFuncSignature(ByteString.EMPTY)
          )
        ),
        List.empty,
        List(
          CallableFunction(
            CallableAnnotation("invocation"),
            Terms.FUNC(
              "foo",
              List("a"),
              LET_BLOCK(
                LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
                FUNCTION_CALL(
                  User(FieldNames.WriteSet),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a").explicitGet(), REF("a"))),
                        FUNCTION_CALL(
                          Native(1100),
                          List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), REF("sender0"))), REF("nil"))
                        )
                      )
                    )
                  )
                )
              )
            )
          ),
          CallableFunction(
            CallableAnnotation("invocation"),
            Terms.FUNC(
              "default",
              List.empty,
              LET_BLOCK(
                LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
                FUNCTION_CALL(
                  User(FieldNames.WriteSet),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a").explicitGet(), CONST_STRING("b").explicitGet())),
                        FUNCTION_CALL(
                          Native(1100),
                          List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), REF("sender0"))), REF("nil"))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        Some(
          VerifierFunction(
            VerifierAnnotation("t"),
            FUNC(
              "verify",
              List.empty,
              FUNCTION_CALL(Native(FunctionIds.EQ), List(GETTER(REF("t"), "id"), CONST_BYTESTR(ByteStr.empty).explicitGet()))
            )
          )
        )
      )
    )
    TestCompiler(V3).compile(
      """
        | @Callable(invocation)
        | func foo(a:ByteVector) = {
        |  let sender0 = invocation.caller.bytes
        |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
        | }
        |
        | @Callable(invocation)
        | func default() = {
        |   let sender0 = invocation.caller.bytes
        |   WriteSet([DataEntry("a", "b"), DataEntry("sender", sender0)])
        | }
        |
        | @Verifier(t)
        | func verify() = {
        |   t.id == base58''
        | }
      """.stripMargin
    ) shouldBe expectedResult
  }

  property("contract with default func compiles") {
    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 1,
          List(
            CallableFuncSignature(ByteString.EMPTY)
          )
        ),
        List.empty,
        List(
          CallableFunction(
            CallableAnnotation("invocation"),
            Terms.FUNC(
              "default",
              List.empty,
              LET_BLOCK(
                LET("sender0", GETTER(GETTER(REF("invocation"), "caller"), "bytes")),
                FUNCTION_CALL(
                  User(FieldNames.WriteSet),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("a").explicitGet(), CONST_STRING("b").explicitGet())),
                        FUNCTION_CALL(
                          Native(1100),
                          List(FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender").explicitGet(), REF("sender0"))), REF("nil"))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        None
      )
    )
    TestCompiler(V3).compile(
      """
        | @Callable(invocation)
        | func default() = {
        |   let sender0 = invocation.caller.bytes
        |   WriteSet([DataEntry("a", "b"), DataEntry("sender", sender0)])
        | }
      """.stripMargin
    ) shouldBe expectedResult
  }

  property("contract compiles callable functions independently") {
    TestCompiler(V3).compile(
      """
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
      """.stripMargin
    ) should produce("Can't find a function 'foo'")
  }

  property("contract can access declarations") {
    TestCompiler(V3).compile(
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
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("contract compiles fails when incorrect return type") {
    import com.wavesplatform.lang.v1.evaluator.ctx.impl.*

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

    compiler.ContractCompiler(ctx, expr, V3) should produce(
      callableResultError(Types.callableReturnType(V3).explicitGet(), "ByteVector", ScriptResultSource.CallableFunction)
    )

    compiler.ContractCompiler(ctx, expr, V4) should produce(
      callableResultError(Types.callableReturnType(V4).explicitGet(), "ByteVector", ScriptResultSource.CallableFunction)
    )
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
    compiler.ContractCompiler(ctx, expr, V3) shouldBe Symbol("right")
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
    compiler.ContractCompiler(ctx, expr, V3) should produce("more than 1 verifier function")
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
    compiler.ContractCompiler(ctx, expr, V3) should produce("Annotation not recognized")
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
    compiler.ContractCompiler(ctx, expr, V3) should produce("must have 0 arguments")
  }

  property("hodlContract") {
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(V3, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, V3).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(V3, Account, DAppType).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
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
    compiler.ContractCompiler(ctx, expr, V3) shouldBe Symbol("right")
  }

  property("contract functions could return parent type values") {
    TestCompiler(V3).compile(
      """
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
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("wavesBalanceV4 have type BalanceDetails") {
    TestCompiler(V4).compile(
      """
        | @Callable(i)
        | func bar() = {
        |   [ScriptTransfer(i.caller, wavesBalance(this), base58'somestr')]
        | }
      """.stripMargin
    ) should produce("Non-matching types: expected: Int, actual: BalanceDetails")
  }

  property("wavesBalanceV4 have type BalanceDetails with fields") {
    val ctx = dAppV4Ctx
    val expr = {
      val script =
        """
          | @Callable(i)
          | func bar() = {
          |   [
          |     ScriptTransfer(i.caller, wavesBalance(this).available, base58'somestr'),
          |     ScriptTransfer(i.caller, wavesBalance(this).regular, base58'somestr'),
          |     ScriptTransfer(i.caller, wavesBalance(this).generating, base58'somestr'),
          |     ScriptTransfer(i.caller, wavesBalance(this).effective, base58'somestr')
          |   ]
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr, V4) shouldBe Symbol("right")
  }

  property("assetBalanceV4 allow issued assets only") {
    val ctx = dAppV4Ctx
    val expr = {
      val script =
        """
          | @Callable(i)
          | func bar() = {
          |   [ScriptTransfer(i.caller, assetBalance(this, unit), base58'somestr')]
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr, V4) should produce("Non-matching types: expected: ByteVector, actual: Unit")
  }

  property("assetBalanceV3 allow issued assets and waves") {
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(V3, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, V3).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(V3, Account, DAppType).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
      .compilerContext
    val expr = {
      val script =
        """
          | @Callable(i)
          | func bar() = {
          |   TransferSet([ScriptTransfer(i.caller, assetBalance(this, unit), base58'somestr'),
          |                ScriptTransfer(i.caller, assetBalance(this, base58'asset'), base58'somestr')])
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr, V3) shouldBe Symbol("right")
  }

  property("contract compilation fails if functions has the same name") {
    TestCompiler(V3).compile(
      """
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
      """.stripMargin
    ) should produce("is already defined")
  }

  property("contract compilation fails if declaration and annotation bindings has the same name") {
    TestCompiler(V3).compile(
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
    ) should produce("already defined")
  }

  property("contract compilation fails if annotation bindings and func args has the same name") {
    TestCompiler(V3).compile(
      """
        |@Callable(i)
        |func some(i: Int) = {
        |   if (this == "abc") then
        |      WriteSet([DataEntry("a", "a")])
        |   else
        |      WriteSet([DataEntry("a", "b")])
        |}
        |
      """.stripMargin
    ) should produce("override annotation bindings")
  }

  property("contract compiles if annotation bindings and another func args has the same name") {
    TestCompiler(V3).compile(
      """
        |@Callable(x)
        |func foo(i: Int) = {
        |    WriteSet([DataEntry("a", "a")])
        |}
        |
        |@Callable(i)
        |func bar(x: Int) = {
        |    WriteSet([DataEntry("a", "a")])
        |}
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("contract compiles if declaration vars and func args has the same name") {
    TestCompiler(V3).compile(
      """
        |let x = 42
        |
        |@Callable(i)
        |func some(x: Int) = {
        |    WriteSet([DataEntry("a", "a")])
        |}
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("contract compiles if it use invoke script fields: payment, feeAssetId") {
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
          |        isDefined(tx.payment) && isDefined(tx.feeAssetId) &&
          |        match tx.args[0] {
          |          case _: Int => false
          |          case _: String => false
          |          case _: Boolean => false
          |          case _: ByteVector => false
          |          case ll => ll[0] == 1
          |        }
          |      case _ => true
          |    }
          |  }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(dAppV3Ctx, expr, V3) shouldBe Symbol("right")
  }

  property("matching case with non-existing type should produce error message with suitable types") {
    val verifierTypes = Types.verifierInput(V3).typeList.map(_.name)
    TestCompiler(V3).compile(
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
    ) should produce(verifierTypes.mkString(", "))
  }

  property("expression matching case with non-existing type should produce error message with suitable types") {
    val verifierTypes = Types.verifierInput(V3).typeList.map(_.name)
    TestCompiler(V3).compile(
      s"""
         | func local(tx: ${verifierTypes.mkString("|")}) = tx
         |
         | @Verifier(tx)
         | func test() =
         |   match local(tx) {
         |     case _: UndefinedType => true
         |     case _                => false
         | }
       """.stripMargin
    ) should produce(verifierTypes.mkString(", "))
  }

  property("matching case with union type containing non-existing type should produce error message with suitable types") {
    val verifierTypes = Types.verifierInput(V3).typeList.map(_.name)
    TestCompiler(V3).compile(
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
    ) should produce(verifierTypes.mkString(", "))
  }

  property("locally call @Callable func should produce informative error") {
    TestCompiler(V3).compile(
      """
        | @Callable(i)
        | func f1(a:ByteVector) = WriteSet([])
        |
        | @Callable(i)
        | func f2(a:ByteVector) = f1(a)
        |
      """.stripMargin
    ) should produce("Can't find a function 'f1'(ByteVector) or it is @Callable")
  }

  property("contract compiles if script uses InvokeScriptTransaction function and args field") {
    TestCompiler(V3).compile(
      s"""
         | @Verifier(tx)
         | func verify() = {
         |   match tx {
         |     case ist: InvokeScriptTransaction => isDefined(ist.function) && isDefined(ist.args)
         |     case _ => false
         |   }
         | }
       """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("compiler error if user function defined below usage") {
    TestCompiler(V3).compile(
      """
        | let a = foo()
        | func foo() = (1)
        |
        | @Verifier(tx)
        | func bar() = { a == 1 }
      """.stripMargin
    ) should produce("Can't find a function")
  }

  property("compiler error if variable defined below usage") {
    TestCompiler(V3).compile(
      """
        | func foo() = (a)
        | let a = 1
        |
        | @Verifier(tx)
        | func bar() = { foo() == 1 }
      """.stripMargin
    ) should produce("A definition of 'a' is not found")
  }

  property("contract compilation fails if function name length is longer than 255 bytes") {
    val longName = "a" * (ContractLimits.MaxDeclarationNameInBytes + 1)
    TestCompiler(V3).compile(
      s"""
         | @Callable(i)
         | func $longName() = {
         |    WriteSet([])
         | }
       """.stripMargin
    ) should produce(s"Function '$longName' size = 256 bytes exceeds 255")
  }

  property("contract compiles if function name length is equal to 255 bytes") {
    val longName = "a" * ContractLimits.MaxDeclarationNameInBytes
    TestCompiler(V3).compile(
      s"""
         | @Callable(i)
         | func $longName() = {
         |    WriteSet([])
         | }
       """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("compiler error if annotated func has argument of not native type") {
    TestCompiler(V3).compile(
      """
        | @Callable(i)
        | func f1(a:Alias) = WriteSet([])
      """.stripMargin
    ) should produce("Unexpected argument type in function")
  }

  property("contract compiles if annotated func has argument of native type") {
    TestCompiler(V3).compile(
      """
        | @Callable(i)
        | func f1(a:Int, b:ByteVector, c:Boolean, d:String) = WriteSet([])
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("list as @Callable argument forbidden in V3") {
    TestCompiler(V3).compile(
      """
        | @Callable(i)
        | func f(a:List[Int]) = WriteSet([])
      """.stripMargin
    ) should produce("Unexpected callable func arg type: List[Int]")
  }

  property("list as @Callable argument allowed in V4") {
    TestCompiler(V4).compile(
      """
        | @Callable(i)
        | func f(a:List[Int]) = []
      """.stripMargin
    ) shouldBe Symbol("right")
  }

  property("@Callable V4 result type") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 4    #-}
          | {-#CONTENT_TYPE    DAPP #-}
          |
          | @Callable(i)
          | func foo(a:ByteVector) =
          |   [
          |     IntegerEntry("key", 1),
          |     BooleanEntry("key", true),
          |     StringEntry("key", "str"),
          |     BinaryEntry("key", base58''),
          |     DeleteEntry("key"),
          |     ScriptTransfer(i.caller, 1, base58''),
          |     Issue("name", "description", 1000, 4, true, unit, 0),
          |     Reissue(base58'', 1, false),
          |     Burn(base58'', 1),
          |     SponsorFee(base58'', 1)
          |   ]
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val ctx =
      PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)

    compiler.ContractCompiler(ctx.compilerContext, expr, V4) shouldBe Symbol("right")
  }

  property("Asset has no name") {
    val ctx = Monoid
      .combineAll(
        Seq(
          PureContext.build(V3, useNewPowPrecision = true).withEnvironment[Environment],
          CryptoContext.build(com.wavesplatform.lang.Global, V3).withEnvironment[Environment],
          WavesContext.build(
            Global,
            DirectiveSet(V3, Account, DAppType).explicitGet(),
            fixBigScriptField = true
          )
        )
      )
      .compilerContext
    val expr = {
      val script =
        """
          |
          |@Verifier(tx)
          |func verify() = assetInfo(base58'').value().name == "qqqq"
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr, V3) should produce("Undefined field `name` of variable of type `Asset`")
  }

  property("Asset has some name") {
    val ctx = dAppV4Ctx
    val expr = {
      val script =
        """
          |
          |@Verifier(tx)
          |func verify() = assetInfo(base58'').value().name == "qqqq"
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    compiler.ContractCompiler(ctx, expr, V4) shouldBe Symbol("right")
  }

  property("JS API compile limit exceeding error") {
    def byteVectorsList(size: Int) = {
      (1 to size).map(_ => s"base64'${ByteStr(new Array[Byte](1000)).base64Raw}'").mkString("[", ", ", "]")
    }

    val dApp =
      s"""
         |
         |@Verifier(tx)
         |func verify() = {
         |  let list = ${byteVectorsList(163)}
         |  true
         |}
         |
      """.stripMargin

    Global.compileContract(
      dApp,
      dAppV4Ctx,
      V4,
      ScriptEstimatorV3(fixOverflow = true, overhead = true),
      needCompaction = false,
      removeUnusedCode = false
    ) should produce("Script is too large: 165187 bytes > 163840 bytes")
  }

  property("@Callable Invoke") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 5    #-}
          | {-#CONTENT_TYPE    DAPP #-}
          |
          | @Callable(i)
          | func bar() = {
          |   ([], "return")
          | }
          |
          | @Callable(i)
          | func foo(a:ByteVector) = {
          |  let r = invoke(this, "bar", [], [])
          |   [
          |     IntegerEntry("key", 1),
          |     BooleanEntry("key", true),
          |     StringEntry("key", "str"),
          |     BinaryEntry("key", base58''),
          |     DeleteEntry("key"),
          |     ScriptTransfer(i.caller, 1, base58''),
          |     Issue("name", "description", 1000, 4, true, unit, 0),
          |     Reissue(base58'', 1, false),
          |     Burn(base58'', 1),
          |     SponsorFee(base58'', 1)
          |   ]
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet(), fixBigScriptField = true)

    compiler.ContractCompiler(ctx.compilerContext, expr, V5) shouldBe Symbol("right")
  }

  property("@Callable Invoke isn't available for V4") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 4    #-}
          | {-#CONTENT_TYPE    DAPP #-}
          |
          | @Callable(i)
          | func bar() = {
          |   ([], "return")
          | }
          |
          | @Callable(i)
          | func foo(a:ByteVector) = {
          |  let r = invoke(this, "bar", [], [])
          |   [
          |     IntegerEntry("key", 1),
          |     BooleanEntry("key", true),
          |     StringEntry("key", "str"),
          |     BinaryEntry("key", base58''),
          |     DeleteEntry("key"),
          |     ScriptTransfer(i.caller, 1, base58''),
          |     Issue("name", "description", 1000, 4, true, unit, 0),
          |     Reissue(base58'', 1, false),
          |     Burn(base58'', 1),
          |     SponsorFee(base58'', 1)
          |   ]
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val ctx =
      PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)

    compiler.ContractCompiler(ctx.compilerContext, expr, V4) shouldBe Symbol("left")
  }

  property("originCaller and originCallerPublicKey fields isn't available for V4") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 4    #-}
          | {-#CONTENT_TYPE    DAPP #-}
          |
          | @Callable(i)
          | func foo() = {
          |   let a = i.originCaller
          |   let b = i.originCallerPublicKey
          |   []
          | }
        """.stripMargin
      Parser.parseContract(script).get.value
    }
    val ctx =
      PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet(), fixBigScriptField = true)

    val result = compiler.ContractCompiler(ctx.compilerContext, expr, V4)
    result should produce("Undefined field `originCaller` of variable of type `Invocation`")
    result should produce("Undefined field `originCallerPublicKey` of variable of type `Invocation`")
  }

  property("matching types error message") {
    TestCompiler(V4).compile(
      """
        |
        | {-# CONTENT_TYPE DAPP    #-}
        | {-# SCRIPT_TYPE  ACCOUNT #-}
        |
        | @Callable(i)
        | func call() = {
        |   let x = assetBalance(this, i.payments[0].assetId)
        |   []
        | }
      """.stripMargin
    ) should produce("Non-matching types: expected: ByteVector, actual: ByteVector|Unit")
  }

  property("JsAPI compiles dApp with no errors") {
    Global.compileContract(
      """
        |
        |func xxx() = {
        |  let some = 1 + 2
        |  some
        |}
        |
        |func yyy() = {
        |  xxx()
        |}
        |
        |let z1 = 1
        |let z2 = xxx() + yyy()
        |let z3 = z1 + z2
        |
        |@Callable(i)
        |func call() = {
        |  let a = xxx()
        |  let b = yyy()
        |  let c = z3
        |  []
        |}
        |
        |@Verifier(t)
        |func v() = {
        |  let a = xxx()
        |  let b = yyy()
        |  let c = z3
        |  true
        |}
      """.stripMargin,
      getTestContext(V4).compilerContext,
      V4,
      ScriptEstimatorV3(fixOverflow = true, overhead = true),
      needCompaction = false,
      removeUnusedCode = false
    ) shouldBe Symbol("right")
  }

  property("union as @Callable argument is allowed in V4,V5") {
    def checkForV(version: StdLibVersion): Assertion =
      TestCompiler(version).compile(scriptWithUnionArg(version)) shouldBe Symbol("right")

    checkForV(V4)
    checkForV(V5)
  }

  property("union as @Callable argument forbidden in V6") {
    TestCompiler(V6).compile(scriptWithUnionArg(V6)) should
      produce(
        "Union type is not allowed for callable function arguments in 101-102; Union type is not allowed for callable function arguments in 148-149"
      )
  }

  property("union as argument of non-@Callable function is allowed in V6") {
    val script =
      """
        |{-# STDLIB_VERSION 6 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |{-# SCRIPT_TYPE ACCOUNT #-}
        |
        |func test(a: List[Int|String], b: Int|String) = []
        |
        |@Callable(i)
        |func f(a: Int) = []
        |""".stripMargin

    TestCompiler(V6).compile(script) shouldBe Symbol("right")
  }

  private def scriptWithUnionArg(version: StdLibVersion): String =
    s"""
       |{-# STDLIB_VERSION ${version.id} #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |@Callable(i)
       |func f(a: List[Int|String]) = []
       |
       |@Callable(i)
       |func g(a: Int|String) = []
       |""".stripMargin
}
