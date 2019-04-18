package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.{Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX, FunctionHeader}
import monix.eval.Coeval

import scala.collection.mutable

package object utils {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val lazyContexts: Map[DirectiveSet, Coeval[CTX]] = {
    val directives = for {
      version    <- DirectiveDictionary[StdLibVersion].all
      cType      <- DirectiveDictionary[ContentType].all
      scriptType <- DirectiveDictionary[ScriptType].all
    } yield DirectiveSet(version, scriptType, cType)

    val environment = new Environment {
      override def height: Long                                                                                    = 0
      override def chainId: Byte                                                                                   = 1: Byte
      override def inputEntity: Environment.InputEntity                                                            = null
      override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
      override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
      override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                                         = ???
      override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = ???
      override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
      override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
      override def tthis: Recipient.Address                                                                        = ???
    }
    directives
      .filter(_.isRight)
      .map(_.explicitGet())
      .map(ds => {
        val ctx = Coeval.evalOnce(
          Monoid.combineAll(
            Seq(
              PureContext.build(ds.stdLibVersion),
              CryptoContext.build(Global),
              WavesContext.build(ds, environment)
            )
          )
        )
        ds -> ctx
      })
      .toMap
  }

  private val lazyFunctionCosts: Map[StdLibVersion, Coeval[Map[FunctionHeader, Coeval[Long]]]] =
    lazyContexts.map(el => (el._1.stdLibVersion, el._2.map(ctx => estimate(el._1.stdLibVersion, ctx.evaluationContext))))

  def functionCosts(version: StdLibVersion): Map[FunctionHeader, Coeval[Long]] = lazyFunctionCosts(version)()

  def estimate(version: StdLibVersion, ctx: EvaluationContext): Map[FunctionHeader, Coeval[Long]] = {
    val costs: mutable.Map[FunctionHeader, Coeval[Long]] = ctx.typeDefs.collect {
      case (typeName, CASETYPEREF(_, fields)) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    }(collection.breakOut)

    ctx.functions.values.foreach { func =>
      val cost = func.costByLibVersion(version)
      costs += func.header -> Coeval.now(cost)
    }

    costs.toMap
  }

  def compilerContext(version: StdLibVersion, cType: ContentType, isAssetScript: Boolean): CompilerContext = {
    val ds = DirectiveSet(version, ScriptType.isAssetScript(isAssetScript), cType).explicitGet()
    lazyContexts(ds)().compilerContext
  }

  val defaultDecompilerContext: DecompilerContext =
    lazyContexts(DirectiveSet(V3, Account, DApp).explicitGet())().decompilerContext

  def varNames(version: StdLibVersion, cType: ContentType): Set[String] =
    compilerContext(version, cType, isAssetScript = false).varDefs.keySet
}
