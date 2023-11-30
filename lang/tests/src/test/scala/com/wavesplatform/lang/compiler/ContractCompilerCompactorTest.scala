package com.wavesplatform.lang.compiler

import cats.syntax.semigroup.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.values.{DApp as DAppType, *}
import com.wavesplatform.lang.directives.{Directive, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.script.{Script, ScriptPreprocessor}
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, CaseObj, FUNC}
import com.wavesplatform.lang.v1.compiler.Types.{ANY, BIGINT, BOOLEAN, LIST, LONG, PARAMETERIZED, PARAMETERIZEDLIST, STRING, UNION}
import com.wavesplatform.lang.v1.compiler.{ContractScriptCompactor, TestCompiler}
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding.Down
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Types, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, GlobalValNames, PureContext}
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, compiler}
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CompactNameAndOriginalNamePair
import com.wavesplatform.test.*
import org.scalatest.Assertion

class ContractCompilerCompactorTest extends PropSpec {
  implicit class DAppExt(dApp: DApp) {
    def compactedSource(stdLibVersion: StdLibVersion = V5): String =
      Script.decompile(ContractScriptImpl(stdLibVersion, dApp.copy(meta = DAppMeta())))._1
  }

  private def fullCtxForV(version: StdLibVersion): CTX[Environment] = {
    val transactionType = Types.buildTransferTransactionType(true)
    val tx              = CaseObj(transactionType, Map("amount" -> CONST_LONG(100000000L)))
    ctxForV(version) |+|
      CryptoContext.build(Global, version).withEnvironment[Environment] |+|
      CTX[NoContext](
        Seq(transactionType),
        Map(("tx", (transactionType, ContextfulVal.pure[NoContext](tx)))),
        Array.empty
      ).withEnvironment[Environment]
  }

  private def ctxForV(version: StdLibVersion): CTX[Environment] =
    PureContext.build(version, useNewPowPrecision = true).withEnvironment[Environment] |+|
      WavesContext.build(Global, DirectiveSet(version, Account, DAppType).explicitGet(), fixBigScriptField = true)

  property("contract script compaction - V4, V5, V6") {

    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        """
          | {-# CONTENT_TYPE DAPP #-}
          |
          | let fooVar = 42
          |
          | func barFunc(barFuncArg1: Int) = 100500 + barFuncArg1
          |
          | @Callable(invocation)
          | func bazCallableFunc(bazCallableFuncArg1: Int, bazCallableFuncArg2: String) = {
          |   let result = barFunc(fooVar) + bazCallableFuncArg1
          |   [
          |     IntegerEntry("integerEntryKey", result),
          |     StringEntry("stringEntryKey", bazCallableFuncArg2)
          |   ]
          | }
          |
        """.stripMargin
      val resultScript = s"{-# STDLIB_VERSION ${version.id} #-}\n" + script
      Parser.parseContract(resultScript).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |let a = 42
                         |
                         |func b (c) = (100500 + c)
                         |
                         |
                         |@Callable(d)
                         |func bazCallableFunc (e,f) = {
                         |    let g = (b(a) + e)
                         |[IntegerEntry("integerEntryKey", g), StringEntry("stringEntryKey", f)]
                         |    }
                         |
                         |""".stripMargin
    }

    checkForV(V4)
    checkForV(V5)
    checkForV(V6)
  }

  property("contract script compaction - V3") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 3 #-}
          | {-# CONTENT_TYPE DAPP #-}
          |
          | let fooVar = 42
          |
          | func barFunc(barFuncArg1: Int) = 100500 + barFuncArg1
          |
          | @Callable(invocation)
          | func bazCallableFunc(bazCallableFuncArg1: Int, bazCallableFuncArg2: String) = {
          |   let result = barFunc(fooVar) + bazCallableFuncArg1
          |   WriteSet([DataEntry("integerEntryKey", result), DataEntry("stringEntryKey", bazCallableFuncArg2)])
          | }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val compilationResult =
      compiler.ContractCompiler(ctxForV(V3).compilerContext, expr, V3, needCompaction = true).explicitGet().compactedSource(V3)
    compilationResult shouldBe """{-# STDLIB_VERSION 3 #-}
                                 |{-# SCRIPT_TYPE ACCOUNT #-}
                                 |{-# CONTENT_TYPE DAPP #-}
                                 |let a = 42
                                 |
                                 |func b (c) = (100500 + c)
                                 |
                                 |
                                 |@Callable(d)
                                 |func bazCallableFunc (e,f) = {
                                 |    let g = (b(a) + e)
                                 |    WriteSet([DataEntry("integerEntryKey", g), DataEntry("stringEntryKey", f)])
                                 |    }
                                 |
                                 |""".stripMargin
  }

  property("contract script compaction - vars with the same name") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${version.id} #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | func barFunc() = {
           |   let fooVar = 40
           |   fooVar
           | }
           |
           | func bazFunc() = {
           |   let fooVar = 2
           |   fooVar
           | }
           |
           | @Verifier(tx)
           | func verify() = {
           |   barFunc() + bazFunc() == 42
           | }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |func a () = {
                         |    let b = 40
                         |    b
                         |    }
                         |
                         |
                         |func c () = {
                         |    let b = 2
                         |    b
                         |    }
                         |
                         |
                         |
                         |@Verifier(d)
                         |func e () = ((a() + c()) == 42)
                         |""".stripMargin
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script compaction - script var has name as compacted name") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${version.id} #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | let fooVar = "some value"
           |
           | @Verifier(tx)
           | func verify() = {
           |   let a = "some value"
           |   fooVar == a
           | }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |let a = "some value"
                         |
                         |
                         |@Verifier(b)
                         |func c () = {
                         |    let d = "some value"
                         |    (a == d)
                         |    }
                         |""".stripMargin
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script decompaction - decompacted result equal to common compiled result") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""
           | {-# STDLIB_VERSION ${version.id} #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | let fooVar = 42
           |
           | func barFunc(barFuncArg1: Int) = 100500 + barFuncArg1
           |
           | @Callable(invocation)
           | func bazCallableFunc(bazCallableFuncArg1: Int, bazCallableFuncArg2: String) = {
           |   let result = barFunc(fooVar) + bazCallableFuncArg1
           |   [
           |     IntegerEntry("integerEntryKey", result),
           |     StringEntry("stringEntryKey", bazCallableFuncArg2)
           |   ]
           | }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val compilationCompactedResult = compiler.ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
      val decompactedResult          = ContractScriptCompactor.decompact(compilationCompactedResult.explicitGet())

      // noinspection RedundantDefaultArgument
      val compilationResult = compiler.ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = false)

      decompactedResult shouldBe compilationResult.explicitGet()
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script decompaction - ContractScriptCompactor must decompact scripts compacted by old compaction algorithm") {
    def expr: Expressions.DAPP = {
      val script =
        s"""
           | {-# STDLIB_VERSION 5 #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | let fooVar = 42
           |
           | func barFunc(barFuncArg1: Int) = 100500 + barFuncArg1
           |
           | @Callable(invocation)
           | func bazCallableFunc(bazCallableFuncArg1: Int, bazCallableFuncArg2: String) = {
           |   let result = barFunc(fooVar) + bazCallableFuncArg1
           |   [
           |     IntegerEntry("integerEntryKey", result),
           |     StringEntry("stringEntryKey", bazCallableFuncArg2)
           |   ]
           | }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    def compactedExpr: Expressions.DAPP = {
      val script =
        s"""
           | {-# STDLIB_VERSION 5 #-}
           | {-# CONTENT_TYPE DAPP #-}
           |
           | let a = 42
           |
           | func b(c: Int) = 100500 + c
           |
           | @Callable(d)
           | func bazCallableFunc(e: Int, f: String) = {
           |   let g = b(a) + e
           |   [
           |     IntegerEntry("integerEntryKey", g),
           |     StringEntry("stringEntryKey", f)
           |   ]
           | }
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val oldCompOriginalNames = Seq(
      "a" -> "fooVar",
      "b" -> "barFunc",
      "c" -> "barFuncArg1",
      "d" -> "invocation",
      "e" -> "bazCallableFuncArg1",
      "f" -> "bazCallableFuncArg2",
      "g" -> "result"
    ).map(p => CompactNameAndOriginalNamePair(p._1, p._2))

    val compilationNotCompactedResult    = compiler.ContractCompiler(ctxForV(V5).compilerContext, expr, V5).explicitGet()
    val compilationCompactedResultNoMeta = compiler.ContractCompiler(ctxForV(V5).compilerContext, compactedExpr, V5).explicitGet()
    val oldCompactedResult = compilationCompactedResultNoMeta.copy(
      meta = compilationCompactedResultNoMeta.meta
        .withCompactNameAndOriginalNamePairList(oldCompOriginalNames)
        .withFuncs(compilationNotCompactedResult.meta.funcs)
    )
    val decompactedResult = ContractScriptCompactor.decompact(oldCompactedResult)

    decompactedResult shouldBe compilationNotCompactedResult
  }

  property("contract script compaction - remove unused code") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""
           |{-# STDLIB_VERSION ${version.id} #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |let varX = 111
           |let varY = 222
           |let varZ = 333
           |
           |func func3() = varZ * 444
           |func func2() = 100500 - varY
           |func func1() = func2() + 42
           |
           |@Callable(i)
           |func call() = {
           |  let tmp1 = func1() + varX
           |  [IntegerEntry("somekey", tmp1)]
           |}
           |
           |@Verifier(tx)
           |func verify() = {
           |  func2() != varX
           |}
           |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true, removeUnusedCode = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |let a = 111
                         |
                         |let b = 222
                         |
                         |func c () = (100500 - b)
                         |
                         |
                         |func d () = (c() + 42)
                         |
                         |
                         |@Callable(e)
                         |func call () = {
                         |    let f = (d() + a)
                         |[IntegerEntry("somekey", f)]
                         |    }
                         |
                         |
                         |@Verifier(g)
                         |func h () = (c() != a)
                         |""".stripMargin
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script with imports - remove unused code") {
    val libraries =
      Map(
        "lib1" ->
          """
            | {-# CONTENT_TYPE LIBRARY #-}
            | let tmp = "some value"
            | func foo() = 40
          """.stripMargin,
        "lib2" ->
          """
            | {-# CONTENT_TYPE LIBRARY #-}
            | func bar() = 2
            | func baz(a: Int, b: Int) = a - b
          """.stripMargin
      )

    def scriptForV(version: StdLibVersion): String =
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE DAPP #-}
         | {-# IMPORT lib1,lib2 #-}
         |
         | @Verifier(tx)
         | func verify() = {
         |  foo() + bar() == 42
         | }
      """.stripMargin

    def checkForV(version: StdLibVersion): Assertion = {
      val declarations = for {
        directives  <- DirectiveParser(scriptForV(version))
        ds          <- Directive.extractDirectives(directives)
        (linked, _) <- ScriptPreprocessor(scriptForV(version), libraries, ds.imports)
        expr = Parser.parseContract(linked).get.value
        r <- compiler.ContractCompiler(ctxForV(version).compilerContext, expr, version, removeUnusedCode = true)
      } yield r.decs

      declarations.explicitGet() should contain.only(
        FUNC("foo", Nil, CONST_LONG(40)),
        FUNC("bar", Nil, CONST_LONG(2))
      )
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script compaction - fix shadowing") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""{-# STDLIB_VERSION ${version.id} #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |func a() = true
           |func b() = a()
           |func bb() = b()
           |
           |@Callable(i)
           |func c() = {
           |  strict r = bb()
           |  []
           |}""".stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |func a () = true
                         |
                         |
                         |func b () = a()
                         |
                         |
                         |func d () = b()
                         |
                         |
                         |@Callable(e)
                         |func c () = {
                         |    let f = d()
                         |    if ((f == f))
                         |        then nil
                         |        else throw("Strict value is not equal to itself.")
                         |    }
                         |
                         |""".stripMargin
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("contract script compaction - many names") {
    def exprForV(version: StdLibVersion): Expressions.DAPP = {
      val script =
        s"""{-# STDLIB_VERSION ${version.id} #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |${(1 to 100).map("func a" + _ + " () = true").mkString("\n")}""".stripMargin
      Parser.parseContract(script).get.value
    }

    def checkForV(version: StdLibVersion): Assertion = {
      val result = compiler
        .ContractCompiler(ctxForV(version).compilerContext, exprForV(version), version, needCompaction = true)
        .explicitGet()
        .compactedSource(version)
      result shouldBe s"""{-# STDLIB_VERSION ${version.id} #-}
                         |{-# SCRIPT_TYPE ACCOUNT #-}
                         |{-# CONTENT_TYPE DAPP #-}
                         |func a () = true
                         |
                         |
                         |func b () = true
                         |
                         |
                         |func c () = true
                         |
                         |
                         |func d () = true
                         |
                         |
                         |func e () = true
                         |
                         |
                         |func f () = true
                         |
                         |
                         |func g () = true
                         |
                         |
                         |func h () = true
                         |
                         |
                         |func i () = true
                         |
                         |
                         |func j () = true
                         |
                         |
                         |func k () = true
                         |
                         |
                         |func l () = true
                         |
                         |
                         |func m () = true
                         |
                         |
                         |func n () = true
                         |
                         |
                         |func o () = true
                         |
                         |
                         |func p () = true
                         |
                         |
                         |func q () = true
                         |
                         |
                         |func r () = true
                         |
                         |
                         |func s () = true
                         |
                         |
                         |func t () = true
                         |
                         |
                         |func u () = true
                         |
                         |
                         |func v () = true
                         |
                         |
                         |func w () = true
                         |
                         |
                         |func x () = true
                         |
                         |
                         |func y () = true
                         |
                         |
                         |func z () = true
                         |
                         |
                         |func A () = true
                         |
                         |
                         |func B () = true
                         |
                         |
                         |func C () = true
                         |
                         |
                         |func D () = true
                         |
                         |
                         |func E () = true
                         |
                         |
                         |func F () = true
                         |
                         |
                         |func G () = true
                         |
                         |
                         |func H () = true
                         |
                         |
                         |func I () = true
                         |
                         |
                         |func J () = true
                         |
                         |
                         |func K () = true
                         |
                         |
                         |func L () = true
                         |
                         |
                         |func M () = true
                         |
                         |
                         |func N () = true
                         |
                         |
                         |func O () = true
                         |
                         |
                         |func P () = true
                         |
                         |
                         |func Q () = true
                         |
                         |
                         |func R () = true
                         |
                         |
                         |func S () = true
                         |
                         |
                         |func T () = true
                         |
                         |
                         |func U () = true
                         |
                         |
                         |func V () = true
                         |
                         |
                         |func W () = true
                         |
                         |
                         |func X () = true
                         |
                         |
                         |func Y () = true
                         |
                         |
                         |func Z () = true
                         |
                         |
                         |func aa () = true
                         |
                         |
                         |func ab () = true
                         |
                         |
                         |func ac () = true
                         |
                         |
                         |func ad () = true
                         |
                         |
                         |func ae () = true
                         |
                         |
                         |func af () = true
                         |
                         |
                         |func ag () = true
                         |
                         |
                         |func ah () = true
                         |
                         |
                         |func ai () = true
                         |
                         |
                         |func aj () = true
                         |
                         |
                         |func ak () = true
                         |
                         |
                         |func al () = true
                         |
                         |
                         |func am () = true
                         |
                         |
                         |func an () = true
                         |
                         |
                         |func ao () = true
                         |
                         |
                         |func ap () = true
                         |
                         |
                         |func aq () = true
                         |
                         |
                         |func ar () = true
                         |
                         |
                         |func as () = true
                         |
                         |
                         |func at () = true
                         |
                         |
                         |func au () = true
                         |
                         |
                         |func av () = true
                         |
                         |
                         |func aw () = true
                         |
                         |
                         |func ax () = true
                         |
                         |
                         |func ay () = true
                         |
                         |
                         |func az () = true
                         |
                         |
                         |func aA () = true
                         |
                         |
                         |func aB () = true
                         |
                         |
                         |func aC () = true
                         |
                         |
                         |func aD () = true
                         |
                         |
                         |func aE () = true
                         |
                         |
                         |func aF () = true
                         |
                         |
                         |func aG () = true
                         |
                         |
                         |func aH () = true
                         |
                         |
                         |func aI () = true
                         |
                         |
                         |func aJ () = true
                         |
                         |
                         |func aK () = true
                         |
                         |
                         |func aL () = true
                         |
                         |
                         |func aM () = true
                         |
                         |
                         |func aN () = true
                         |
                         |
                         |func aO () = true
                         |
                         |
                         |func aP () = true
                         |
                         |
                         |func aQ () = true
                         |
                         |
                         |func aR () = true
                         |
                         |
                         |func aS () = true
                         |
                         |
                         |func aT () = true
                         |
                         |
                         |func aU () = true
                         |
                         |
                         |func aV () = true
                         |
                         |
                         |""".stripMargin
    }

    checkForV(V5)
    checkForV(V6)
  }

  property("removeUnusedCode mode should not remove global functions definitions used in inner functions") {
    val compiled = TestCompiler(V6).compile(
      """
        |func user() = []
        |
        |@Callable(i)
        |func default() = {
        |  func f() = user()
        |  f()
        |}
      """.stripMargin,
      removeUnused = true
    )
    compiled.explicitGet().decs.map(_.name) shouldBe List("user")
  }

  property("compaction should work correctly with global variables") {
    def script(varName: String): String =
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |func f($varName: Any) = $varName
         |
         |@Callable(i)
         |func e() = {
         |    ( [], f($varName) )
         |}
        """.stripMargin

    GlobalValNames.All.foreach { globalName =>
      val expr = Parser.parseContract(script(globalName)).get.value
      val dApp = compiler.ContractCompiler(fullCtxForV(V6).compilerContext, expr, V6, needCompaction = true).explicitGet()
      dApp.compactedSource(V6) shouldBe
        s"""{-# STDLIB_VERSION 6 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |func a (b) = b
           |
           |
           |@Callable(c)
           |func e () = $$Tuple2(nil, a($globalName))
           |
           |""".stripMargin

      ContractScriptCompactor.decompact(dApp) shouldBe compiler.ContractCompiler(fullCtxForV(V6).compilerContext, expr, V6).explicitGet()
    }
  }

  property("compaction should work correctly with global user functions") {
    val globalUserFunctions =
      fullCtxForV(V6).functions
        .filter(f => f.header.isInstanceOf[User] && f.name.head.isLetter)
        .map { func =>
          func.name -> func.signature.args.map { case (_, tp) =>
            tp match {
              case PARAMETERIZEDLIST(_) | LIST(_)                     => "nil"
              case ANY | BIGINT | LONG | _: PARAMETERIZED             => "1"
              case STRING                                             => "\"1\""
              case BOOLEAN                                            => "true"
              case UNION(types, _) if types.contains(Down.`type`)     => "DOWN"
              case UNION(types, _) if types.contains(Types.aliasType) => "Alias(\"123\")"
              case _                                                  => ???
            }
          }
        }

    def script(funcName: String, funcArgs: Seq[String]): String =
      s"""
         |{-# STDLIB_VERSION 6 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |func f($funcName: Any) = $funcName
         |
         |@Callable(i)
         |func e() = {
         |    ( [], $funcName(${funcArgs.mkString(",")}) )
         |}
        """.stripMargin

    globalUserFunctions.foreach { case (funcName, args) =>
      val expr = Parser.parseContract(script(funcName, args)).get.value
      val dApp = compiler.ContractCompiler(fullCtxForV(V6).compilerContext, expr, V6, needCompaction = true).explicitGet()
      dApp.compactedSource(V6) shouldBe
        s"""{-# STDLIB_VERSION 6 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |func a (b) = b
           |
           |
           |@Callable(c)
           |func e () = $$Tuple2(nil, $funcName(${args.mkString(", ")}))
           |
           |""".stripMargin

      ContractScriptCompactor.decompact(dApp) shouldBe compiler.ContractCompiler(fullCtxForV(V6).compilerContext, expr, V6).explicitGet()
    }
  }
}
