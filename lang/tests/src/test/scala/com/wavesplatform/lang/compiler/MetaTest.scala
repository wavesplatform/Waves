package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.meta.{MetaMapper, ParsedMeta}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, V3, V4, DApp => DAppType}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MetaTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink with Inside {
  property("meta v1 with union type parameters") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          Global, 
          DirectiveSet(V3, Account, DAppType).explicitGet()
        )
        .compilerContext
    )
    val expr = {
      val script =
        """
          |
          | @Callable(invocation)
          | func foo(
          |   a: Int|String|ByteVector|Boolean,
          |   b: Int|Int,
          |   c: String|Int,
          |   d: Int|String,
          |   e: String|ByteVector|Boolean,
          |   f: ByteVector|Int
          | ) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
          | }
          |
          | @Callable(invocation)
          | func bar(
          |   a: Int|Boolean
          | ) = {
          |  let sender0 = invocation.caller.bytes
          |  WriteSet([DataEntry("a", a), DataEntry("sender", sender0)])
          | }
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val expectedMeta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](15, 1, 9, 9, 14, 3))),
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](5))),
      )
    )

    val dApp = compiler.ContractCompiler(ctx, expr, V3).explicitGet()
    dApp.meta shouldBe expectedMeta

    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(
        version = 1,
        Some(
          List(
            List(
              UNION(BOOLEAN, BYTESTR, LONG, STRING),
              LONG,
              UNION(LONG, STRING),
              UNION(LONG, STRING),
              UNION(BOOLEAN, BYTESTR, STRING),
              UNION(BYTESTR, LONG)
            ),
            List(
              UNION(BOOLEAN, LONG)
            )
          )
        )
      )
    )
  }

  property("meta v1 with empty-param function") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          Global,
          DirectiveSet(V3, Account, DAppType).explicitGet()
        )
        .compilerContext
    )
    val expr = {
      val script =
        """
          |
          | {-# STDLIB_VERSION 3 #-}
          | {-# CONTENT_TYPE DAPP #-}
          | {-# SCRIPT_TYPE ACCOUNT #-}
          |
          | @Callable(i)
          | func default() = WriteSet([])
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val expectedMeta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature(ByteString.EMPTY),
      )
    )

    val dApp = compiler.ContractCompiler(ctx, expr, V3).explicitGet()
    dApp.meta shouldBe expectedMeta

    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(version = 1, Some(List(Nil)))
    )
  }

  property("meta v2 supporting list parameters") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext.build(Global, DirectiveSet(V4, Account, DAppType).explicitGet())
        .compilerContext
    )

    val expr = {
      val script =
        """
          |
          | @Callable(i)
          | func foo(a: List[Int], b: List[String], c: ByteVector, d: Int|String|Boolean) = []
          |
          | @Callable(i)
          | func bar(a: List[ByteVector]) = []
          |
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val expectedMeta = DAppMeta(
      version = 2,
      List(
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](17, 24, 2, 13))),
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](18))),
      )
    )

    val dApp = compiler.ContractCompiler(ctx, expr, V4).explicitGet()

    dApp.meta shouldBe expectedMeta
    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(
        version = 2,
        Some(
          List(
            List(
              LIST(LONG),
              LIST(STRING),
              BYTESTR,
              UNION(BOOLEAN, LONG, STRING)
            ),
            List(
              LIST(BYTESTR)
            )
          )
        )
      )
    )
  }

  property("meta v2 with empty-param function") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          Global,
          DirectiveSet(V4, Account, DAppType).explicitGet()
        )
        .compilerContext
    )
    val expr = {
      val script =
        """
          |
          | {-# STDLIB_VERSION 4 #-}
          | {-# CONTENT_TYPE DAPP #-}
          | {-# SCRIPT_TYPE ACCOUNT #-}
          |
          | @Callable(i)
          | func default() = []
        """.stripMargin
      Parser.parseContract(script).get.value
    }

    val expectedMeta = DAppMeta(
      version = 2,
      List(
        CallableFuncSignature(ByteString.EMPTY),
      )
    )

    val dApp = compiler.ContractCompiler(ctx, expr, V4).explicitGet()
    dApp.meta shouldBe expectedMeta

    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(
        version = 2,
        Some(List(Nil))
      )
    )
  }
}
