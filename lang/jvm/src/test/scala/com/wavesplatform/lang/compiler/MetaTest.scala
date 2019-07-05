package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.meta.{Chain, Dic, MetaMapper, Single}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V3}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class MetaTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  property("meta with union type parameters") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
          DirectiveSet(V3, Account, DApp).explicitGet(),
          Common.emptyBlockchainEnvironment()
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

    val meta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature("foo", ByteString.copyFrom(Array[Byte](15, 1, 9, 9, 14, 3))),
        CallableFuncSignature("bar", ByteString.copyFrom(Array[Byte](5))),
      )
    )
    val expectedResult = Right(meta)
    compiler.ContractCompiler(ctx, expr).map(_.meta) shouldBe expectedResult

    MetaMapper.dicFromProto(meta) shouldBe Right(
      Dic(Map("callableFuncTypes" ->
        Chain(List(
          Dic(Map(
            "name" -> Single("foo"),
            "types" -> Chain(List(
              Single("Boolean|Int|String"),
              Single("Int"),
              Single("Int|String"),
              Single("Int|String"),
              Single("Boolean|String"),
              Single("ByteVector|Int")
            ))
          )),
          Dic(Map(
            "name" -> Single("bar"),
            "types" -> Chain(List(
              Single("Boolean|Int")
            ))
          ))
        ))
      ))
    )
  }
}
