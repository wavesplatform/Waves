package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.tx.TransferTransaction
import com.wavesplatform.lang.v1.repl.model.{Account, ByteString, WithSignature}

case class TransferTransactionV1(
    id: ByteString,
    signature: ByteString,
    recipient: String,
    amount: Long,
    assetId: String,
    feeAssetId: String,
    attachment: ByteString,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    senderPublicKey: Account
) extends TransferTransaction with WithSignature
