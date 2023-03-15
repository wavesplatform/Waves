package com.wavesplatform.lang.v1.repl.node.http.response

import com.wavesplatform.lang.v1.traits.domain.Recipient as LangRecipient

object TransferTxSerializer {

  private val typeId: Byte = 4

  def bodyBytes(
      sender: Array[Byte],
      assetId: Option[Array[Byte]],
      feeAssetId: Option[Array[Byte]],
      timestamp: Long,
      amount: Long,
      fee: Long,
      recipient: LangRecipient,
      attachment: Array[Byte],
      version: Byte,
      chainId: Byte,
      proofs: List[Array[Byte]]
  ): Array[Byte] = ???
}
