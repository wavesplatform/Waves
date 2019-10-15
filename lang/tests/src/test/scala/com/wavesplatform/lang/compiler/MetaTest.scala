package com.wavesplatform.lang.compiler

import cats.kernel.Monoid
import com.google.protobuf.ByteString
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp.{CallableAnnotation, CallableFunction}
import com.wavesplatform.lang.contract.meta.{Chain, Dic, MetaMapper, Single}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, V3, DApp => DAppType}
import com.wavesplatform.lang.v1.compiler
import com.wavesplatform.lang.v1.compiler.Terms.{FUNC, REF}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.protobuf.dapp.DAppMeta
import com.wavesplatform.protobuf.dapp.DAppMeta.CallableFuncSignature
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.collection.immutable.{ListMap, Map}

class MetaTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink with Inside {
  property("meta with union type parameters") {
    val ctx = Monoid.combine(
      compilerContext,
      WavesContext
        .build(
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

    val meta = DAppMeta(
      version = 1,
      List(
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](15, 1, 9, 9, 14, 3))),
        CallableFuncSignature(ByteString.copyFrom(Array[Byte](5))),
      )
    )
    compiler.ContractCompiler(ctx, expr).map(_.meta) shouldBe Right(meta)

    val callables = List(
      CallableFunction(CallableAnnotation("invocation"), FUNC("foo", List("a", "b", "c", "d", "e", "f"), REF(""))),
      CallableFunction(CallableAnnotation("invocation"), FUNC("bar", List("a"), REF("")))
    )
    val dApp = DApp(meta, Nil, callables, None)
    MetaMapper.dicFromProto(dApp) shouldBe Right(
      Dic(Map(
        "version" -> Single("1"),
        "callableFuncTypes" -> Chain(List(
          Dic(ListMap(
            "a" -> Single("Boolean|Int|String"),
            "b" -> Single("Int"),
            "c" -> Single("Int|String"),
            "d" -> Single("Int|String"),
            "e" -> Single("Boolean|String"),
            "f" -> Single("ByteVector|Int")
          )),
          Dic(Map(
            "a" -> Single("Boolean|Int")
          ))
        ))
      ))
    )
  }
}
