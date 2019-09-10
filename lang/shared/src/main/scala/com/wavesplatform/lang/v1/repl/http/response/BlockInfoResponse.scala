package com.wavesplatform.lang.v1.repl.http.response

private[http] case class BlockInfoResponse(
  timestamp: Long,
  height: Int,
  `nxt-consensus`: NxtData,
  generator: ByteString
)

private[http] case class NxtData(
  `base-target`: Long,
  `generation-signature`: ByteString
)
