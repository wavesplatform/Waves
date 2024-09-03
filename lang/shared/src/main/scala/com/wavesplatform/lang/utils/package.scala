package com.wavesplatform.lang

import cats.{Id, Monoid}
import cats.syntax.traverse.*
import cats.implicits.catsSyntaxSemigroup
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.compiler.{CompilerContext, DecompilerContext}
import com.wavesplatform.lang.v1.evaluator.{FunctionIds, Log}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.traits.domain.Recipient.Address
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.lang.v1.{BaseGlobal, CTX, FunctionHeader}
import monix.eval.Coeval

import scala.collection.mutable

package object utils {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val environment: Environment[Id] = buildEnvironment(ByteStr.empty)

  def buildEnvironment(txIdParam: ByteStr): Environment[Id] = new Environment[Id] {
    override def height: Long                                                                                    = 0
    override def chainId: Byte                                                                                   = 1: Byte
    override def inputEntity: Environment.InputEntity                                                            = null
    override val txId: ByteStr                                                                                   = txIdParam
    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer]                                   = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
    override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo]                                         = ???
    override def lastBlockOpt(): Option[BlockInfo]                                                               = ???
    override def blockInfoByHeight(height: Int): Option[BlockInfo]                                               = ???
    override def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]                   = None
    override def hasData(addressOrAlias: Recipient): Boolean                                                     = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
    override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails]    = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    override def tthis: Environment.Tthis                                              = Coproduct(Recipient.Address(ByteStr.empty))
    override def multiPaymentAllowed: Boolean                                          = true
    override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer]     = ???
    override def addressFromString(address: String): Either[String, Recipient.Address] = ???
    override def addressFromPublicKey(publicKey: ByteStr): Either[String, Address]     = ???
    override def accountScript(addressOrAlias: Recipient): Option[Script]              = ???
    override def calculateDelay(gt: ByteStr, b: Long): Long                            = ???
    override def callScript(
        dApp: Address,
        func: String,
        args: List[EVALUATED],
        payments: Seq[(Option[Array[Byte]], Long)],
        availableComplexity: Int,
        reentrant: Boolean
    ): Coeval[(Either[ValidationError, (EVALUATED, Log[Id])], Int)] = ???
  }

  val lazyContexts: Map[(DirectiveSet, Boolean, Boolean), Coeval[CTX[Environment]]] =
    (for {
      version     <- DirectiveDictionary[StdLibVersion].all
      scriptType  <- DirectiveDictionary[ScriptType].all
      contentType <- DirectiveDictionary[ContentType].all if contentType != DApp || (contentType == DApp && version >= V3 && scriptType == Account)
      useNewPowPrecision <- Seq(false, true)
      fixBigScriptField  <- Seq(false, true)
    } yield {
      val ds = DirectiveSet(version, scriptType, contentType).explicitGet()
      val ctx = Coeval.evalOnce(
        PureContext.build(version, useNewPowPrecision).withEnvironment[Environment] |+|
          CryptoContext.build(Global, version).withEnvironment[Environment] |+|
          WavesContext.build(Global, ds, fixBigScriptField)
      )
      (ds, useNewPowPrecision, fixBigScriptField) -> ctx
    }).toMap

  private val lazyFunctionCosts: Map[DirectiveSet, Coeval[Map[FunctionHeader, Coeval[Long]]]] =
    lazyContexts.map(el => (el._1._1, el._2.map(ctx => estimate(el._1._1.stdLibVersion, ctx.evaluationContext[Id](environment)))))

  private val dAppVerifierProhibitedFunctions: Map[Native, Coeval[Long]] =
    List(Native(FunctionIds.CALLDAPP), Native(FunctionIds.CALLDAPPREENTRANT))
      .map(f => (f, Coeval.raiseError[Long](new RuntimeException("DApp-to-dApp invocations are not allowed from verifier"))))
      .toMap

  private val dAppVerifierFunctionCosts: Map[StdLibVersion, Map[FunctionHeader, Coeval[Long]]] =
    lazyFunctionCosts.collect {
      case (ds, functions) if ds.contentType == DApp =>
        if (ds.stdLibVersion >= V5)
          (ds.stdLibVersion, functions() ++ dAppVerifierProhibitedFunctions)
        else
          (ds.stdLibVersion, functions())
    }

  private val combinedContext: Map[(StdLibVersion, ContentType), CTX[Environment]] =
    lazyContexts
      .groupBy { case (ds, _) =>
        (ds._1.stdLibVersion, ds._1.contentType)
      }
      .view
      .mapValues(
        _.toList
          .map(_._2)
          .sequence
          .map(Monoid.combineAll[CTX[Environment]])()
      )
      .toMap

  private val combinedFunctionCosts: Map[(StdLibVersion, ContentType), Map[FunctionHeader, Coeval[Long]]] =
    lazyFunctionCosts
      .groupBy { case (ds, _) =>
        (ds.stdLibVersion, ds.contentType)
      }
      .view
      .mapValues(
        _.toList
          .map(_._2)
          .sequence
          .map(_.foldLeft(Map.empty[FunctionHeader, Coeval[Long]])(_ ++ _))()
      )
      .toMap

  def functionCosts(
      version: StdLibVersion,
      contentType: ContentType = Expression,
      scriptType: ScriptType = Account,
      isDAppVerifier: Boolean = false,
      withCombinedContext: Boolean = false
  ): Map[FunctionHeader, Coeval[Long]] =
    if (isDAppVerifier)
      dAppVerifierFunctionCosts(version)
    else if (withCombinedContext)
      combinedFunctionCosts(DirectiveSet(version, scriptType, contentType).explicitGet())
    else
      functionCosts(DirectiveSet(version, scriptType, contentType).explicitGet())

  def functionCosts(ds: DirectiveSet): Map[FunctionHeader, Coeval[Long]] =
    lazyFunctionCosts(ds)()

  def combinedFunctionCosts(ds: DirectiveSet): Map[FunctionHeader, Coeval[Long]] =
    combinedFunctionCosts((ds.stdLibVersion, ds.contentType))

  def estimate(version: StdLibVersion, ctx: EvaluationContext[Environment, Id]): Map[FunctionHeader, Coeval[Long]] = {
    val costs: mutable.Map[FunctionHeader, Coeval[Long]] = mutable.Map.from(ctx.typeDefs.collect {
      case (typeName, CASETYPEREF(_, fields, hidden)) if (!hidden || version < V4) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    })

    ctx.functions.values.foreach { func =>
      val cost = func.costByLibVersion(version)
      costs += func.header -> Coeval.now(cost)
    }

    costs.toMap
  }

  def ctx(version: Int, isTokenContext: Boolean, isContract: Boolean): CTX[Environment] = {
    val ds = DirectiveSet(
      DirectiveDictionary[StdLibVersion].idMap(version),
      ScriptType.isAssetScript(isTokenContext),
      if (isContract) DApp else Expression
    )
    lazyContexts((ds.explicitGet(), true, true)).value()
  }

  def compilerContext(version: StdLibVersion, cType: ContentType, isAssetScript: Boolean): CompilerContext = {
    val ds = DirectiveSet(version, ScriptType.isAssetScript(isAssetScript), cType).explicitGet()
    compilerContext(ds)
  }

  def compilerContext(ds: DirectiveSet): CompilerContext =
    lazyContexts((ds.copy(imports = Imports()), true, true))().compilerContext

  def getDecompilerContext(v: StdLibVersion, cType: ContentType): DecompilerContext =
    combinedContext((v, cType)).decompilerContext

  def varNames(version: StdLibVersion, cType: ContentType): Set[String] =
    compilerContext(version, cType, isAssetScript = false).varDefs.keySet

  def combinedVarNames(version: StdLibVersion, cType: ContentType): Set[String] =
    combinedContext((version, cType)).compilerContext.varDefs.keySet
}
