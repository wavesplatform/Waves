package com.wavesplatform.lang.v1.repl.model

case class Block(
  timestamp: Long,
  height: Int,
  `nxt-consensus`: NxtData,
  generator: ByteString
)

case class NxtData(
  `base-target`: Long,
  `generation-signature`: ByteString
)
