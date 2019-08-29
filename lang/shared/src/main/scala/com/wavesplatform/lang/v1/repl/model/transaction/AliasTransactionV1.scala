package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, Alias, WithSignature}

case class AliasTransactionV1(
    id: ByteString,
    senderPublicKey: Account,
    alias: Alias,
    fee: Long,
    timestamp: Long,
    height: Int,
    `type`: Byte,
    version: Byte,
    signature: ByteString
) extends AliasTransaction with WithSignature
