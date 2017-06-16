package com.wavesplatform.network

import java.net.InetSocketAddress

import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.transaction.History


sealed trait Message

case object GetPeers extends Message
case class KnownPeers(peers: Seq[InetSocketAddress]) extends Message

case class GetSignatures(signatures: Seq[ByteStr]) extends Message
case class Signatures(signatures: Seq[ByteStr]) extends Message

case class GetBlock(signature: ByteStr) extends Message

case class LocalScoreChanged(newLocalScore: History.BlockchainScore) extends Message

case class RawBytes(code: Byte, data: Array[Byte]) extends Message

case class LoadBlockchainExtension(lastBlockIds: Seq[ByteStr])
case class ExtensionIds(lastCommonId: ByteStr, extensionIds: Seq[ByteStr])
case class ExtensionBlocks(extension: Seq[Block])

case class BlockForged(block: Block)
