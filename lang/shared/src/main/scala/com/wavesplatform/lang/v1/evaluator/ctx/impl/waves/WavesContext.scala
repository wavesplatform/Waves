package com.wavesplatform.lang.v1.evaluator.ctx.impl.waves

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.v1.CTX
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Functions._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Vals._
import com.wavesplatform.lang.v1.traits._

object WavesContext {
  def build(ds: DirectiveSet): CTX[Environment] =
    invariableCtx |+| variableCtxCache(ds)

  private val commonFunctions =
    Array(
      txHeightByIdF,
      getIntegerFromStateF,
      getBooleanFromStateF,
      getBinaryFromStateF,
      getStringFromStateF,
      addressFromPublicKeyF,
      addressFromStringF,
      addressFromRecipientF,
      assetBalanceF,
      wavesBalanceF
    )

  private val invariableCtx =
    CTX(Seq(), Map(height), commonFunctions)

  private val allDirectives =
    for {
      version     <- DirectiveDictionary[StdLibVersion].all
      scriptType  <- DirectiveDictionary[ScriptType].all
      contentType <- DirectiveDictionary[ContentType].all
    } yield DirectiveSet(version, scriptType, contentType)

  private val variableCtxCache: Map[DirectiveSet, CTX[Environment]] =
    allDirectives
      .filter(_.isRight)
      .map(_.explicitGet())
      .map(ds => (ds, variableCtx(ds)))
      .toMap

  private def variableCtx(ds: DirectiveSet): CTX[Environment] = {
    val isTokenContext = ds.scriptType match {
      case Account => false
      case Asset   => true
    }
    val proofsEnabled = !isTokenContext
    val version = ds.stdLibVersion
    CTX(
      variableTypes(version, proofsEnabled),
      variableVars(isTokenContext, version, ds.contentType, proofsEnabled),
      variableFuncs(version, ds.contentType, proofsEnabled)
    )
  }

  private def fromV3Funcs(v: StdLibVersion) =
    extractedFuncs(v) ++ Array(assetInfoF(v), blockInfoByHeightF(v), stringFromAddressF)

  private def variableFuncs(version: StdLibVersion, c: ContentType, proofsEnabled: Boolean) = {
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
      )
    lazy val v4Funcs = fromV3Funcs(version) :+ transferTxByIdF(proofsEnabled, version) :+ calculateAssetIdF
    val versionSpecificFuncs =
      version match {
        case V1 | V2 => Array(txByIdF(proofsEnabled, version))
        case V3      => fromV3Funcs(version) :+ transferTxByIdF(proofsEnabled, version)
        case V4      => v4Funcs
     }
    commonFuncs ++ versionSpecificFuncs
  }

  private def variableVars(
    isTokenContext: Boolean,
    version:        StdLibVersion,
    contentType:    ContentType,
    proofsEnabled:  Boolean
  ) = {
    val txVal = tx(isTokenContext, version, proofsEnabled)
    version match {
      case V1 => Map(txVal)
      case V2 => Map(sell, buy, txVal)
      case V3 | V4 =>
        val `this` = if (isTokenContext) assetThis(version) else accountThis
        val txO    = if (contentType == Expression) Map(txVal) else Map()
        val common = Map(sell, buy, lastBlock(version), `this`)
        common ++ txO
    }
  }

  private def variableTypes(version: StdLibVersion, proofsEnabled: Boolean) =
    buildWavesTypes(proofsEnabled, version)           ++
    (if (version >= V3) dAppTypes(version) else Nil)  ++
    (if (version >= V4) List(blockHeader)  else Nil)
}
