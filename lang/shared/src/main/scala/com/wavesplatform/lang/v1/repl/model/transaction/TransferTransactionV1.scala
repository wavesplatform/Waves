package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.tx.TransferTransaction
import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithSignature}

case class TransferTransactionV1(
    id: ByteString,
    recipient: String,
    amount: Long,
    assetId: Option[String],
    feeAssetId: Option[String],
    attachment: ByteString,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    senderPublicKey: Account,
    signature: ByteString
) extends TransferTransaction {
    override def proofs: List[ByteString] = List(signature)
}
