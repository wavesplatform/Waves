package com.wavesplatform.transaction.serialization.impl

import java.nio.ByteBuffer
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.Serde
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, FUNCTION_CALL}
import com.wavesplatform.serialization._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{Asset, TxAmount, TxVersion}
import play.api.libs.json.{JsArray, JsObject, JsString, Json}

import scala.util.Try

object InvokeScriptTxSerializer {
  def functionCallToJson(fc: Terms.FUNCTION_CALL): JsObject = {
    Json.obj(
      "function" -> JsString(fc.function.asInstanceOf[com.wavesplatform.lang.v1.FunctionHeader.User].internalName),
      "args" -> JsArray(
        fc.args.map {
          case Terms.ARR(elements) => Json.obj("type" -> "list", "value" -> elements.map(mapSingleArg))
          case other               => mapSingleArg(other)
        }
      )
    )
  }

  private def mapSingleArg(arg: EXPR) =
    arg match {
      case Terms.CONST_LONG(num)      => Json.obj("type" -> "integer", "value" -> num)
      case Terms.CONST_BOOLEAN(bool)  => Json.obj("type" -> "boolean", "value" -> bool)
      case Terms.CONST_BYTESTR(bytes) => Json.obj("type" -> "binary", "value" -> bytes.base64)
      case Terms.CONST_STRING(str)    => Json.obj("type" -> "string", "value" -> str)
      case arg                        => throw new NotImplementedError(s"Not supported: $arg")
    }

  def toJson(tx: InvokeScriptTransaction): JsObject = {
    import tx._
    BaseTxJson.toJson(tx) ++ Json.obj(
      "dApp"    -> dAppAddressOrAlias.stringRepr,
      "payment" -> payments
    ) ++ (funcCallOpt match {
      case Some(fc) => Json.obj("call" -> this.functionCallToJson(fc))
      case None     => JsObject.empty
    })
  }

  def bodyBytes(tx: InvokeScriptTransaction): Array[Byte] = {
    import tx._
    version match {
      case TxVersion.V1 =>
        Bytes.concat(
          Array(builder.typeId, version, chainId),
          sender.arr,
          dAppAddressOrAlias.bytes,
          Deser.serializeOption(funcCallOpt)(Serde.serialize(_)),
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
    if (tx.isProtobufVersion) PBTransactionSerializer.bytes(tx)
    else Bytes.concat(Array(0: Byte), this.bodyBytes(tx), tx.proofs.bytes())

  def parseBytes(bytes: Array[Byte]): Try[InvokeScriptTransaction] = Try {
    def parsePayment(arr: Array[Byte]): Payment = {
      val amt               = Longs.fromByteArray(arr.take(8))
      val (maybeAssetId, _) = Deser.parseOption(arr, 8, 32)(ByteStr.apply)
      val asset             = maybeAssetId.fold[Asset](Waves)(IssuedAsset)
      Payment(amt, asset)
    }

    val buf = ByteBuffer.wrap(bytes)
    require(buf.getByte == 0 && buf.getByte == InvokeScriptTransaction.typeId && buf.getByte == 1, "transaction type mismatch")
    val chainId = buf.getByte
    require(chainId == AddressScheme.current.chainId, "chainId mismatch")

    val sender       = buf.getPublicKey
    val dApp         = buf.getAddressOrAlias
    val functionCall = Deser.parseOption(buf)(Serde.deserialize(_).explicitGet().asInstanceOf[FUNCTION_CALL])
    val payments     = Deser.parseArrays(buf).map(parsePayment)
    val fee          = TxAmount.unsafeFrom(buf.getLong)
    val feeAssetId   = buf.getAsset
    val timestamp    = buf.getLong
    InvokeScriptTransaction(TxVersion.V1, sender, dApp, functionCall, payments, fee, feeAssetId, timestamp, buf.getProofs, chainId)
  }
}
