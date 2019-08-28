package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, FunctionCall, WithId, WithProofs}

case class InvokeScriptTransaction(
    id: ByteString,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    proofs: List[ByteString],
    senderPublicKey: Account,
    dApp: String,
    call: FunctionCall
) extends Transaction with WithProofs with WithId
