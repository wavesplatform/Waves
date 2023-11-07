package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.serialization.SerdeV1
import com.wavesplatform.serialization.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Asset, PBSince, TxPositiveAmount, TxVersion}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import scala.util.Try

object InvokeScriptTxSerializer {
  def functionCallToJson(fc: Terms.FUNCTION_CALL): JsObject = {
    Json.obj(
      "function" -> JsString(fc.function.funcName),
      "args" -> JsArray(
        fc.args.map {
          case Terms.ARR(elements) => Json.obj("type" -> "list", "value" -> elements.map(mapSingleArg))
          case other               => mapSingleArg(other)
        }
      )
    )
  }

  private def mapSingleArg(arg: EXPR): JsObject =
    arg match {
      case Terms.CONST_LONG(num)      => Json.obj("type" -> "integer", "value" -> num)
      case Terms.CONST_BOOLEAN(bool)  => Json.obj("type" -> "boolean", "value" -> bool)
      case Terms.CONST_BYTESTR(bytes) => Json.obj("type" -> "binary", "value" -> bytes.base64)
      case Terms.CONST_STRING(str)    => Json.obj("type" -> "string", "value" -> str)
      case Terms.ARR(_) =>
        Json.obj(
          "type"  -> "list",
          "value" -> "unsupported"
        ) // should not be shown on normal cases, added only to avoid NotImplementedError while constructing error for illegal callable argument type
      case arg => throw new NotImplementedError(s"Not supported: $arg")
    }

  def toJson(tx: InvokeScriptTransaction): JsObject = BaseTxJson.toJson(tx) ++ Json.obj(
    "dApp"    -> tx.dApp.toString,
    "payment" -> tx.payments
  ) ++ Json.obj("call" -> InvokeScriptTxSerializer.functionCallToJson(tx.funcCall))

  def bodyBytes(tx: InvokeScriptTransaction): Array[Byte] = {
    import tx.*
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(tpe.id.toByte, version, chainId),
          sender.arr,
          dApp.bytes,
          Deser.serializeOption(funcCallOpt)(SerdeV1.serialize(_)),
          Deser.serializeArrays(payments.map(pmt => Longs.toByteArray(pmt.amount) ++ pmt.assetId.byteRepr)),
          Longs.toByteArray(fee.value),
          feeAssetId.byteRepr,
          Longs.toByteArray(timestamp)
        )

      case _ =>
        PBTransactionSerializer.bodyBytes(tx)
    }
  }

  def toBytes(tx: InvokeScriptTransaction): Array[Byte] =
    if (PBSince.affects(tx)) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())

  def parseBytes(bytes: Array[Byte]): Try[InvokeScriptTransaction] = Try {
    def parsePayment(arr: Array[Byte]): Payment = {
      val amt               = Longs.fromByteArray(arr.take(8))
      val (maybeAssetId, _) = Deser.parseOption(arr, 8, 32)(ByteStr.apply)
      val asset             = Asset.fromCompatId(maybeAssetId)
      Payment(amt, asset)
    }

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == InvokeScriptTransaction.typeId && buf.getByte == 1, "transaction type mismatch")
    val chainId = buf.getByte

    val sender       = buf.getPublicKey
    val dApp         = buf.getAddressOrAlias(chainId)
    val functionCall = Deser.parseOption(buf)(SerdeV1.deserializeFunctionCall(_).explicitGet())
    val payments     = Deser.parseArrays(buf).map(parsePayment)
    val fee          = TxPositiveAmount.unsafeFrom(buf.getLong)
    val feeAssetId   = buf.getAsset
    val timestamp    = buf.getLong
    InvokeScriptTransaction(TxVersion.V1, sender, dApp, functionCall, payments, fee, feeAssetId, timestamp, buf.getProofs, chainId)
  }
}
