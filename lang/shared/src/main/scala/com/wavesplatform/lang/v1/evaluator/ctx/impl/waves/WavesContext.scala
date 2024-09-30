package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.syntax.semigroup.*
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Functions.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Vals.*
import com.wavesplatform.lang.v1.traits.*
import com.wavesplatform.lang.v1.{BaseGlobal, CTX}

object WavesContext {
  def build(global: BaseGlobal, ds: DirectiveSet, fixBigScriptField: Boolean): CTX[Environment] =
    invariableCtx |+| variableCtx(global, ds, fixBigScriptField)

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

  private def variableCtx(global: BaseGlobal, ds: DirectiveSet, fixBigScriptField: Boolean): CTX[Environment] = {
    val isTokenContext = ds.scriptType == Asset
    val proofsEnabled  = !isTokenContext
    val version        = ds.stdLibVersion
    val types          = variableTypes(version, proofsEnabled)
    val typeDefs       = types.view.map(t => t.name -> t).toMap
    CTX(
      types,
      variableVars(isTokenContext, version, ds.contentType, proofsEnabled, fixBigScriptField),
      variableFuncs(global, ds, typeDefs, proofsEnabled)
    )
  }

  private def fromV3Funcs(proofsEnabled: Boolean, v: StdLibVersion, typeDefs: Map[String, FINAL]) =
    extractedFuncs(v) ++ Array(
      assetInfoF(v, typeDefs),
      blockInfoByHeightF(v, typeDefs),
      transferTxByIdF(proofsEnabled, v, typeDefs),
      stringFromAddressF
    )

  private def fromV4Funcs(proofsEnabled: Boolean, version: StdLibVersion, typeDefs: Map[String, FINAL]) =
    fromV3Funcs(proofsEnabled, version, typeDefs) ++ Array(
      calculateAssetIdF,
      transactionFromProtoBytesF(proofsEnabled, version, typeDefs),
      simplifiedIssueActionConstructor,
      detailedIssueActionConstructor
    ) ++ balanceV4Functions

  private def fromV5Funcs(proofsEnabled: Boolean, ds: DirectiveSet, typeDefs: Map[String, FINAL]) = {
    val v5Funcs = Array(
      simplifiedLeaseActionConstructor,
      detailedLeaseActionConstructor,
      calculateLeaseId,
      isDataStorageUntouchedF
    )

    val dAppFuncs =
      if (ds.contentType == DApp || ds.scriptType == Call)
        Array(callDAppF(reentrant = false), callDAppF(reentrant = true))
      else
        Array[BaseFunction[Environment]]()

    val accountFuncs =
      if (ds.scriptType == Account)
        selfCallFunctions(V5)
      else
        Array[BaseFunction[Environment]]()

    fromV4Funcs(proofsEnabled, ds.stdLibVersion, typeDefs) ++ v5Funcs ++ dAppFuncs ++ accountFuncs
  }

  private def fromV8Funcs(proofsEnabled: Boolean, ds: DirectiveSet, typeDefs: Map[String, FINAL]) =
    fromV5Funcs(proofsEnabled, ds, typeDefs) :+ calculateDelay

  private def selfCallFunctions(v: StdLibVersion) =
    Array(
      getIntegerFromStateSelfF,
      getBooleanFromStateSelfF,
      getBinaryFromStateSelfF,
      getStringFromStateSelfF
    ) ++ extractedStateSelfFuncs(v)

  private def variableFuncs(global: BaseGlobal, ds: DirectiveSet, typeDefs: Map[String, FINAL], proofsEnabled: Boolean) = {
    val version = ds.stdLibVersion
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
      ) ++ (if (version >= V5) Array(accountScriptHashF(global)) else Array.empty[BaseFunction[Environment]])

    val versionSpecificFuncs =
      version match {
        case V1 | V2      => Array(txByIdF(proofsEnabled, version)) ++ balanceV123Functions
        case V3           => fromV3Funcs(proofsEnabled, version, typeDefs) ++ balanceV123Functions
        case V4           => fromV4Funcs(proofsEnabled, version, typeDefs)
        case V5 | V6 | V7 => fromV5Funcs(proofsEnabled, ds, typeDefs)
        case _            => fromV8Funcs(proofsEnabled, ds, typeDefs)
      }
    commonFuncs ++ versionSpecificFuncs
  }

  private def variableVars(
      isTokenContext: Boolean,
      version: StdLibVersion,
      contentType: ContentType,
      proofsEnabled: Boolean,
      fixBigScriptField: Boolean
  ): Map[String, (FINAL, ContextfulVal[Environment])] = {
    val txVal = tx(isTokenContext, version, proofsEnabled, fixBigScriptField)
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
