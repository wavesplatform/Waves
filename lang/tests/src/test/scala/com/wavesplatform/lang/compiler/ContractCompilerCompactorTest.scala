package com.wavesplatform.lang.compiler

import cats.syntax.semigroup._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.{Directive, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.script.{Script, ScriptPreprocessor}
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.ContractScriptCompactor
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.test._

class ContractCompilerCompactorTest extends PropSpec {
  implicit class DAppExt(dApp: DApp) {
    def compactedSource(stdLibVersion: StdLibVersion = V5): String =
      Script.decompile(ContractScriptImpl(stdLibVersion, dApp.copy(meta = DAppMeta())))._1
  }

  property("contract script compaction - V4, V5") {
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
    val exprV4 = {
      val v4Script = "{-# STDLIB_VERSION 4 #-}\n" + script
      Parser.parseContract(v4Script).get.value
    }

    val exprV5 = {
      val v5Script = "{-# STDLIB_VERSION 5 #-}\n" + script
      Parser.parseContract(v5Script).get.value
    }

    val ctxV4 =
      PureContext.build(V4, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet())

    val ctxV5 =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    def expectedScript(stdLibVersion: StdLibVersion): String =
      s"""{-# STDLIB_VERSION ${stdLibVersion.id} #-}
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

    compiler.ContractCompiler(ctxV4.compilerContext, exprV4, V4, needCompaction = true).explicitGet().compactedSource(V4) shouldBe expectedScript(V4)
    compiler.ContractCompiler(ctxV5.compilerContext, exprV5, V5, needCompaction = true).explicitGet().compactedSource(V5) shouldBe expectedScript(V5)
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

    val stdLibVer = V3
    val ctx =
      PureContext.build(stdLibVer, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(stdLibVer, Account, DAppType).explicitGet())

    val compilationResult =
      compiler.ContractCompiler(ctx.compilerContext, expr, stdLibVer, needCompaction = true).explicitGet().compactedSource(stdLibVer)
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
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 5 #-}
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

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true).explicitGet().compactedSource(V5)
    compilationResult shouldBe """{-# STDLIB_VERSION 5 #-}
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

  property("contract script compaction - script var has name as compacted name") {
    val expr = {
      val script =
        """
          | {-# STDLIB_VERSION 5 #-}
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

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true).explicitGet().compactedSource(V5)
    compilationResult shouldBe """{-# STDLIB_VERSION 5 #-}
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

  property("contract script decompaction - decompacted result equal to common compiled result") {
    val expr = {
      val script =
        """
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

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationCompactedResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true)
    val decompactedResult          = ContractScriptCompactor.decompact(compilationCompactedResult.explicitGet())

    //noinspection RedundantDefaultArgument
    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = false)

    decompactedResult shouldBe compilationResult.explicitGet()
  }

  property("contract script compaction - remove unused code") {
    val expr = {
      val script =
        """
          |{-# STDLIB_VERSION 5 #-}
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

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult =
      compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true, removeUnusedCode = true).explicitGet().compactedSource()
    compilationResult shouldBe """{-# STDLIB_VERSION 5 #-}
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

    val script =
      """
        | {-# STDLIB_VERSION 5 #-}
        | {-# CONTENT_TYPE DAPP #-}
        | {-# IMPORT lib1,lib2 #-}
        |
        | @Verifier(tx)
        | func verify() = {
        |  foo() + bar() == 42
        | }
      """.stripMargin

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    //noinspection RedundantDefaultArgument
    val compilationResult = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      linked     <- ScriptPreprocessor(script, libraries, ds.imports)
      expr = Parser.parseContract(linked).get.value
      r <- compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = false, removeUnusedCode = true)
    } yield r.compactedSource()

    compilationResult.explicitGet() shouldBe """{-# STDLIB_VERSION 5 #-}
                                               |{-# SCRIPT_TYPE ACCOUNT #-}
                                               |{-# CONTENT_TYPE DAPP #-}
                                               |func bar () = 2
                                               |
                                               |
                                               |func foo () = 40
                                               |
                                               |
                                               |
                                               |@Verifier(tx)
                                               |func verify () = ((foo() + bar()) == 42)
                                               |""".stripMargin
  }

  property("contract script compaction - fix shadowing") {
    val expr = {
      val script =
        """{-# STDLIB_VERSION 5 #-}
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

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationCompactedResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true).explicitGet().compactedSource()
    compilationCompactedResult shouldBe """{-# STDLIB_VERSION 5 #-}
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

  property("contract script compaction - many names") {
    val expr = {
      val script =
        s"""{-# STDLIB_VERSION 5 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          |${(1 to 100).map("func a" + _ + " () = true").mkString("\n")}""".stripMargin
      Parser.parseContract(script).get.value
    }

    val ctx =
      PureContext.build(V5, useNewPowPrecision = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationCompactedResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true).explicitGet().compactedSource()
    compilationCompactedResult shouldBe """{-# STDLIB_VERSION 5 #-}
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
}
