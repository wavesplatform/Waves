package com.wavesplatform.network

import scorex.block.Block
import scorex.transaction.History


sealed trait Message

case object GetPeers extends Message
case class GetSignatures(signatures: Block.BlockIds) extends Message
case class Signatures(signatures: Block.BlockIds) extends Message
case class GetBlock(signature: Block.BlockId) extends Message

case class RawBytes(code: Byte, data: Array[Byte]) extends Message

case class ExtensionIds(lastCommonId: Block.BlockId, extensionIds: Block.BlockIds)
case class ExtensionBlocks(extension: Seq[Block])
case class LocalScoreChanged(newLocalScore: History.BlockchainScore)