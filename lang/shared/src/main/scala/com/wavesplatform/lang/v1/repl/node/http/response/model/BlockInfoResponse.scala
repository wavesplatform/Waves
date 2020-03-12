package com.wavesplatform.lang.v1.repl.node.http.response.model

private[node] case class BlockInfoResponse(
  timestamp: Long,
  height: Int,
  `nxt-consensus`: NxtData,
  generator: ByteString,
  VRF: Option[ByteString],
  transactionsRoot: ByteString
)

private[node] case class NxtData(
  `base-target`: Long,
  `generation-signature`: ByteString
)
