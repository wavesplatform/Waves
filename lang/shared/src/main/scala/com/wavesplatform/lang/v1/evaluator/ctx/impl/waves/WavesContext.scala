package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.syntax.semigroup._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Functions.{addressFromStringF, _}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Vals._
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

object WavesContext {
  def build(global: BaseGlobal, ds: DirectiveSet): CTX[Environment] =
    invariableCtx |+| variableCtxCache(global, ds)

  private val commonFunctions =
    Array(
      txHeightByIdF,
      getIntegerFromStateF,
      getBooleanFromStateF,
      getBinaryFromStateF,
      getStringFromStateF,
      addressFromRecipientF
    )

  private val balanceV123Functions =
    Array(
      assetBalanceF,
      wavesBalanceF
    )

  private val balanceV4Functions =
    Array(
      assetBalanceV4F,
      wavesBalanceV4F
    )

  private val invariableCtx =
    CTX(Seq(), Map(height), commonFunctions)

  private val ctxCache = scala.collection.mutable.AnyRefMap.empty[(BaseGlobal, DirectiveSet), CTX[Environment]]

  private def variableCtxCache(global: BaseGlobal, ds: DirectiveSet) =
    ctxCache.getOrElse((global, ds), ctxCache.synchronized {
      ctxCache.getOrElseUpdate((global, ds), variableCtx(global, ds))
    })

  private def variableCtx(global: BaseGlobal, ds: DirectiveSet): CTX[Environment] = {
    val isTokenContext = ds.scriptType == Asset
    val proofsEnabled  = !isTokenContext
    val version        = ds.stdLibVersion
    CTX(
      variableTypes(version, proofsEnabled),
      variableVars(isTokenContext, version, ds.contentType, proofsEnabled),
      variableFuncs(global, version, ds.scriptType, ds.contentType, proofsEnabled)
    )
  }

  private def fromV3Funcs(proofsEnabled: Boolean, v: StdLibVersion) =
    extractedFuncs(v) ++ Array(
      assetInfoF(v),
      blockInfoByHeightF(v),
      transferTxByIdF(proofsEnabled, v),
      stringFromAddressF
    )

  private def fromV4Funcs(proofsEnabled: Boolean, version: StdLibVersion) =
    fromV3Funcs(proofsEnabled, version) ++ Array(
      calculateAssetIdF,
      transactionFromProtoBytesF(proofsEnabled, version),
      simplifiedIssueActionConstructor,
      detailedIssueActionConstructor
    ) ++ balanceV4Functions

  private def fromV5Funcs(proofsEnabled: Boolean, version: StdLibVersion, contentType: ContentType, scriptType: ScriptType) = {
    val v5Funcs = Array(
      simplifiedLeaseActionConstructor,
      detailedLeaseActionConstructor,
      calculateLeaseId,
      isDataStorageUntouchedF
    )

    val dAppFuncs =
      if (contentType == DApp || scriptType == Call)
        Array(callDAppF(reentrant = false), callDAppF(reentrant = true))
      else
        Array[BaseFunction[Environment]]()

    val accountFuncs =
      if (scriptType == Account)
        selfCallFunctions(V5)
      else
        Array[BaseFunction[Environment]]()

    fromV4Funcs(proofsEnabled, version) ++ v5Funcs ++ dAppFuncs ++ accountFuncs
  }

  private def selfCallFunctions(v: StdLibVersion) =
    Array(
      getIntegerFromStateSelfF,
      getBooleanFromStateSelfF,
      getBinaryFromStateSelfF,
      getStringFromStateSelfF
    ) ++ extractedStateSelfFuncs(v)

  private def variableFuncs(global: BaseGlobal, version: StdLibVersion, scriptType: ScriptType, contentType: ContentType, proofsEnabled: Boolean) = {
    val commonFuncs =
      Array(
        getIntegerFromArrayF(version),
        getBooleanFromArrayF(version),
        getBinaryFromArrayF(version),
        getStringFromArrayF(version),
        getIntegerByIndexF(version),
        getBooleanByIndexF(version),
        getBinaryByIndexF(version),
        getStringByIndexF(version),
        if (version >= V4) addressFromStringV4 else addressFromStringF(version),
        if (version >= V6) addressFromPublicKeyNative else addressFromPublicKeyF(version)
      ) ++ (if (version >= V5) Array(accountScriptHashF(global)) else Array())

    val versionSpecificFuncs =
      version match {
        case V1 | V2 => Array(txByIdF(proofsEnabled, version)) ++ balanceV123Functions
        case V3      => fromV3Funcs(proofsEnabled, version) ++ balanceV123Functions
        case V4      => fromV4Funcs(proofsEnabled, version)
        case V5      => fromV5Funcs(proofsEnabled, version, contentType, scriptType)
        case V6      => fromV5Funcs(proofsEnabled, version, contentType, scriptType)
      }
    commonFuncs ++ versionSpecificFuncs
  }

  private def variableVars(
      isTokenContext: Boolean,
      version: StdLibVersion,
      contentType: ContentType,
      proofsEnabled: Boolean
  ): Map[String, (FINAL, ContextfulVal[Environment])] = {
    val txVal = tx(isTokenContext, version, proofsEnabled)
    version match {
      case V1 => Map(txVal)
      case V2 => Map(sell, buy, txVal)
      case _ =>
        val `this` = if (isTokenContext) assetThis(version) else accountThis
        val txO    = if (contentType == Expression) Map(txVal) else Map()
        val common = Map(sell, buy, lastBlock(version), `this`)
        common ++ txO
    }
  }

  private def variableTypes(version: StdLibVersion, proofsEnabled: Boolean) =
    buildWavesTypes(proofsEnabled, version) ++
      (if (version >= V3) dAppTypes(version) else Nil)
}
