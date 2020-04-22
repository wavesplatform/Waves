package com.wavesplatform.transaction.smart

import cats.implicits._
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.serialization.impl.ContinuationTxSerializer
import com.wavesplatform.transaction.validation.TxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class ContinuationTransaction(
    expr: EXPR,
    invokeScriptTransactionId: ByteStr,
    timestamp: TxTimestamp
) extends Transaction
    with VersionedTransaction {

  override val builder                        = ContinuationTransaction
  override val json: Coeval[JsObject]         = Coeval(builder.serializer.toJson(this))
  override val bodyBytes: Coeval[Array[Byte]] = json.map(_.toString.getBytes())
  override val bytes: Coeval[Array[Byte]]     = bodyBytes

  override def chainId: Byte =
    AddressScheme.current.chainId

  override def version: TxVersion =
    TxVersion.V1

  override val id: Coeval[ByteStr] =
    Coeval.now(FastHashId.create(invokeScriptTransactionId.arr))

  override def assetFee: (Asset, Long) =
    (Waves, 0)
}

object ContinuationTransaction extends TransactionParser {
  val serializer = ContinuationTxSerializer

  override type TransactionT = ContinuationTransaction

  override def typeId: TxType = 18: Byte

  override def supportedVersions: Set[TxVersion] = Set(TxVersion.V1)

  override def parseBytes(bytes: Array[Byte]): Try[ContinuationTransaction] = ???

  override implicit def validator: TxValidator[ContinuationTransaction] = _.validNel
}
