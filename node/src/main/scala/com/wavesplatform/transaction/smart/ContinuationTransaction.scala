package com.wavesplatform.transaction.smart

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.validation.TxValidator
import monix.eval.Coeval
import play.api.libs.json.JsObject

import scala.util.Try

case class ContinuationTransaction(
    expr: EXPR,
    invokeScriptTransactionId: ByteStr
) extends Transaction
    with FastHashId
    with TxWithFee.InCustomAsset {

  override val builder: TransactionParser          = ContinuationTransaction
  override val bytes: Coeval[Array[TxVersion]]     = ???
  override val json: Coeval[JsObject]              = ???
  override val bodyBytes: Coeval[Array[TxVersion]] = ???

  override def timestamp: TxTimestamp = ???

  override def chainId: TxVersion = ???

  override def feeAssetId: Asset = ???

  override def fee: TxAmount = ???

  override def proofs: Proofs = ???

  override val sender: PublicKey = ???
}

object ContinuationTransaction extends TransactionParser {
  override type TransactionT = ContinuationTransaction

  override def typeId: TxType = 18: Byte

  override def supportedVersions: Set[TxVersion] = Set(1)

  override def parseBytes(bytes: Array[Byte]): Try[ContinuationTransaction] = ???

  override implicit def validator: TxValidator[ContinuationTransaction] = ???
}
