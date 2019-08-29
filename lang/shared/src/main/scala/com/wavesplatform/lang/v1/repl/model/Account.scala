package com.wavesplatform.lang.v1.repl.model

case class Account(
  chainId: Byte,
  publicKey: Array[Byte],
  address: String
)
