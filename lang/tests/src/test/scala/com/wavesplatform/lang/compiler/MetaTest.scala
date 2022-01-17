package com.wavesplatform.lang.compiler

import com.google.protobuf.ByteString
import com.wavesplatform.lang.contract.meta.{MetaMapper, ParsedMeta}
import com.wavesplatform.lang.directives.values.{V3, V4}
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import com.wavesplatform.test.*
import org.scalatest.Inside

class MetaTest extends PropSpec with Inside {
  property("meta v1 with union type parameters") {
    val dApp =
      TestCompiler(V3)
        .compileContract(
          """
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
        """.stripMargin
        )
        .expr

    val expectedMeta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](15, 1, 9, 9, 14, 3))),
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](5)))
      )
    )

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
    val dApp = TestCompiler(V3)
      .compileContract(
        """
          | @Callable(i)
          | func default() = WriteSet([])
      """.stripMargin
      )
      .expr

    val expectedMeta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature(ByteString.EMPTY)
      )
    )

    dApp.meta shouldBe expectedMeta

    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(version = 1, Some(List(Nil)))
    )
  }

  property("meta v2 supporting list parameters") {
    val dApp =
      TestCompiler(V4)
        .compileContract(
          """
            | @Callable(i)
            | func foo(a: List[Int], b: List[String], c: ByteVector, d: Int|String|Boolean) = []
            |
            | @Callable(i)
            | func bar(a: List[ByteVector]) = []
        """.stripMargin
        )
        .expr

    val expectedMeta = DAppMeta(
      version = 2,
      List(
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](17, 24, 2, 13))),
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](18)))
      )
    )

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
    val dApp =
      TestCompiler(V4)
        .compileContract(
          """
            | @Callable(i)
            | func default() = []
          """.stripMargin
        )
        .expr

    val expectedMeta = DAppMeta(
      version = 2,
      List(
        CallableFuncSignature(ByteString.EMPTY)
      )
    )

    dApp.meta shouldBe expectedMeta

    MetaMapper.dicFromProto(dApp) shouldBe Right(
      ParsedMeta(
        version = 2,
        Some(List(Nil))
      )
    )
  }
}
