package com.wavesplatform.lang.v1.repl.model.transactions

import com.wavesplatform.lang.v1.repl.model.{Account, WithProofs}

object IssueTransactionV2 {
  val ISSUE = 3
  private val MAX_TX_SIZE = 1024
}

case class IssueTransactionV2(
  name           : String,
  description    : String,
  quantity       : Long,
  decimals       : Byte,
  isReissuable   : Boolean,
  proofs         : List[ByteString],
  senderPublicKey: Account,
  id             : ByteString,
  fee            : Long,
  timestamp      : Long,
  height         : Int,
  `type`         : Byte,
  version        : Byte
) extends IssueTransaction with WithProofs