package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.primitives.Ints
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.{ContinuationTxSerializer, PBTransactionSerializer}
import com.wavesplatform.transaction.validation.TxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class ContinuationTransaction(
    invokeScriptTransactionId: ByteStr,
    timestamp: TxTimestamp,
    nonce: Int,
    fee: TxAmount,
    feeAssetId: Asset
) extends Transaction
    with VersionedTransaction
    with TxWithFee.InCustomAsset {

  override val builder                        = ContinuationTransaction
  override val json: Coeval[JsObject]         = Coeval(builder.serializer.toJson(this))
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(PBTransactionSerializer.bodyBytes(this))
  override val bytes: Coeval[Array[Byte]]     = Coeval.evalOnce(PBTransactionSerializer.bytes(this))

  override def chainId: Byte =
    AddressScheme.current.chainId

  override def version: TxVersion =
    TxVersion.V1

  override val id: Coeval[ByteStr] =
    Coeval.now(FastHashId.create(invokeScriptTransactionId.arr ++ Ints.toByteArray(nonce)))
}

object ContinuationTransaction extends TransactionParser {
  val serializer = ContinuationTxSerializer

  override type TransactionT = ContinuationTransaction

  override def typeId: TxType = 18: Byte

  override def supportedVersions: Set[TxVersion] = Set(TxVersion.V1)

  override def parseBytes(bytes: Array[Byte]): Try[ContinuationTransaction] = ???

  override implicit def validator: TxValidator[ContinuationTransaction] = _.validNel
}
