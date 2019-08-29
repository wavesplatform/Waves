package com.wavesplatform.lang.v1.repl.model.transaction

import com.wavesplatform.lang.v1.repl.model.{Account, WithSignature}

object IssueTransactionV1 {
  val ISSUE = 3
  private val MAX_TX_SIZE = 10 * 1024
}

case class IssueTransactionV1(
  name           : String,
  description    : String,
  quantity       : Long,
  decimals       : Byte,
  isReissuable   : Boolean,
  signature      : ByteString,
  senderPublicKey: Account,
  id             : ByteString,
  fee            : Long,
  timestamp      : Long,
  height         : Int,
  `type`         : Byte,
  version        : Byte
) extends IssueTransaction with WithSignature