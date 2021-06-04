package com.wavesplatform.lang.compiler

import cats.implicits._
import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.directives.values.{DApp => DAppType, _}
import com.wavesplatform.lang.directives.{Directive, DirectiveParser, DirectiveSet}
import com.wavesplatform.lang.script.ScriptPreprocessor
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{ContractScriptCompactor, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{FieldNames, WavesContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.{CallableFuncSignature, CompactNameAndOriginalNamePair}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractCompilerCompactorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
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

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 2,
          List(
            CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 8)))
          ),
          Seq(
            ("a1", "fooVar"),
            ("a2", "barFunc"),
            ("a3", "barFuncArg1"),
            ("a4", "invocation"),
            ("a5", "bazCallableFuncArg1"),
            ("a6", "bazCallableFuncArg2"),
            ("a7", "result")
          ).map(el => CompactNameAndOriginalNamePair(el._1, el._2))
        ),
        List(
          LET("a1", CONST_LONG(42L)),
          Terms.FUNC(
            "a2",
            List("a3"),
            FUNCTION_CALL(Native(100), List(CONST_LONG(100500L), REF("a3")))
          )
        ),
        List(
          CallableFunction(
            CallableAnnotation("a4"),
            Terms.FUNC(
              "bazCallableFunc",
              List("a5", "a6"),
              LET_BLOCK(
                LET(
                  "a7",
                  FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("a2"), List(REF("a1"))), REF("a5")))
                ),
                FUNCTION_CALL(
                  Native(1100),
                  List(
                    FUNCTION_CALL(User("IntegerEntry"), List(CONST_STRING("integerEntryKey").explicitGet(), REF("a7"))),
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("StringEntry"), List(CONST_STRING("stringEntryKey").explicitGet(), REF("a6"))),
                        REF("nil")
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

    val ctxV4 =
      PureContext.build(V4, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet())

    val ctxV5 =
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    compiler.ContractCompiler(ctxV4.compilerContext, exprV4, V4, needCompaction = true) shouldBe expectedResult
    compiler.ContractCompiler(ctxV5.compilerContext, exprV5, V5, needCompaction = true) shouldBe expectedResult
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

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 1,
          List(
            CallableFuncSignature(ByteString.copyFrom(Array[Byte](1, 8)))
          ),
          Seq(
            ("a1", "fooVar"),
            ("a2", "barFunc"),
            ("a3", "barFuncArg1"),
            ("a4", "invocation"),
            ("a5", "bazCallableFuncArg1"),
            ("a6", "bazCallableFuncArg2"),
            ("a7", "result")
          ).map(el => CompactNameAndOriginalNamePair(el._1, el._2))
        ),
        List(
          LET("a1", CONST_LONG(42L)),
          Terms.FUNC(
            "a2",
            List("a3"),
            FUNCTION_CALL(Native(100), List(CONST_LONG(100500L), REF("a3")))
          )
        ),
        List(
          CallableFunction(
            CallableAnnotation("a4"),
            Terms.FUNC(
              "bazCallableFunc",
              List("a5", "a6"),
              LET_BLOCK(
                LET(
                  "a7",
                  FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("a2"), List(REF("a1"))), REF("a5")))
                ),
                FUNCTION_CALL(
                  User(FieldNames.WriteSet),
                  List(
                    FUNCTION_CALL(
                      Native(1100),
                      List(
                        FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("integerEntryKey").explicitGet(), REF("a7"))),
                        FUNCTION_CALL(
                          Native(1100),
                          List(
                            FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("stringEntryKey").explicitGet(), REF("a6"))),
                            REF("nil")
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
        None
      )
    )

    val stdLibVer = V3
    val ctx =
      PureContext.build(stdLibVer, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(stdLibVer, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, stdLibVer, needCompaction = true)
    compilationResult shouldBe expectedResult
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

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 2,
          List.empty,
          Seq(
            ("a1", "barFunc"),
            ("a2", "fooVar"),
            ("a3", "bazFunc"),
            ("a4", "verify")
          ).map(el => CompactNameAndOriginalNamePair(el._1, el._2))
        ),
        List(
          Terms.FUNC(
            "a1",
            List.empty,
            LET_BLOCK(
              LET("a2", CONST_LONG(40L)),
              REF("a2")
            )
          ),
          Terms.FUNC(
            "a3",
            List.empty,
            LET_BLOCK(
              LET("a2", CONST_LONG(2L)),
              REF("a2")
            )
          )
        ),
        List.empty,
        Some(
          VerifierFunction(
            VerifierAnnotation("tx"),
            Terms.FUNC(
              "a4",
              List(),
              FUNCTION_CALL(
                Native(0),
                List(
                  FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("a1"), List()), FUNCTION_CALL(User("a3"), List()))),
                  CONST_LONG(42L)
                )
              )
            )
          )
        )
      )
    )

    val ctx =
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true)
    compilationResult shouldBe expectedResult
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
          |   let a1 = "some value"
          |   fooVar == a1
          | }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 2,
          List.empty,
          Seq(
            ("a1", "fooVar"),
            ("a2", "verify"),
            ("a3", "a1")
          ).map(el => CompactNameAndOriginalNamePair(el._1, el._2))
        ),
        List(
          LET("a1", CONST_STRING("some value").explicitGet())
        ),
        List.empty,
        Some(
          VerifierFunction(
            VerifierAnnotation("tx"),
            Terms.FUNC(
              "a2",
              List(),
              LET_BLOCK(
                LET("a3", CONST_STRING("some value").explicitGet()),
                FUNCTION_CALL(
                  Native(0),
                  List(
                    REF("a1"),
                    REF("a3")
                  )
                )
              )
            )
          )
        )
      )
    )

    val ctx =
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true)
    compilationResult shouldBe expectedResult
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
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationCompactedResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true)
    val decompactedResult          = ContractScriptCompactor.decompact(compilationCompactedResult.explicitGet())

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

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 2,
          List(
            CallableFuncSignature(ByteString.copyFrom(Array[Byte]()))
          ),
          Seq(
            ("a1", "varX"),
            ("a2", "varY"),
            ("a3", "func2"),
            ("a4", "func1"),
            ("a5", "tmp1"),
            ("a6", "verify")
          ).map(el => CompactNameAndOriginalNamePair(el._1, el._2))
        ),
        List(
          LET("a1", CONST_LONG(111L)),
          LET("a2", CONST_LONG(222L)),
          Terms.FUNC(
            "a3",
            List(),
            FUNCTION_CALL(Native(101), List(CONST_LONG(100500L), REF("a2")))
          ),
          Terms.FUNC(
            "a4",
            List(),
            FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("a3"), List()), CONST_LONG(42L)))
          )
        ),
        List(
          CallableFunction(
            CallableAnnotation("i"),
            Terms.FUNC(
              "call",
              List(),
              LET_BLOCK(
                LET(
                  "a5",
                  FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("a4"), List()), REF("a1")))
                ),
                FUNCTION_CALL(
                  Native(1100),
                  List(
                    FUNCTION_CALL(User("IntegerEntry"), List(CONST_STRING("somekey").explicitGet(), REF("a5"))),
                    REF("nil")
                  )
                )
              )
            )
          )
        ),
        Some(
          VerifierFunction(
            VerifierAnnotation("tx"),
            Terms.FUNC(
              "a6",
              List(),
              FUNCTION_CALL(User("!="), List(FUNCTION_CALL(User("a3"), List()), REF("a1")))
            )
          )
        )
      )
    )

    val ctx =
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = true, removeUnusedCode = true)
    compilationResult shouldBe expectedResult
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

    val expectedResult = Right(
      DApp(
        DAppMeta(
          version = 2,
          List.empty,
          Seq.empty
        ),
        List(
          Terms.FUNC(
            "bar",
            List.empty,
            CONST_LONG(2L)
          ),
          Terms.FUNC(
            "foo",
            List.empty,
            CONST_LONG(40L)
          )
        ),
        List.empty,
        Some(
          VerifierFunction(
            VerifierAnnotation("tx"),
            Terms.FUNC(
              "verify",
              List(),
              FUNCTION_CALL(
                Native(0),
                List(
                  FUNCTION_CALL(Native(100), List(FUNCTION_CALL(User("foo"), List()), FUNCTION_CALL(User("bar"), List()))),
                  CONST_LONG(42L)
                )
              )
            )
          )
        )
      )
    )

    val ctx =
      PureContext.build(V5, fixUnicodeFunctions = true).withEnvironment[Environment] |+|
        WavesContext.build(Global, DirectiveSet(V5, Account, DAppType).explicitGet())

    val compilationResult = for {
      directives <- DirectiveParser(script)
      ds         <- Directive.extractDirectives(directives)
      linked     <- ScriptPreprocessor(script, libraries, ds.imports)
      expr = Parser.parseContract(linked).get.value
      r <- compiler.ContractCompiler(ctx.compilerContext, expr, V5, needCompaction = false, removeUnusedCode = true)
    } yield r

    compilationResult shouldBe expectedResult
  }
}
