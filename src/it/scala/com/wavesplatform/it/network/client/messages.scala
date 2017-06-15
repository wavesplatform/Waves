package com.wavesplatform.it.network.client

import java.net.InetSocketAddress

import com.wavesplatform.state2.ByteStr


sealed trait Message

case object GetPeers extends Message
case class KnownPeers(peers: Seq[InetSocketAddress]) extends Message

case class GetSignatures(signatures: Seq[ByteStr]) extends Message
case class Signatures(signatures: Seq[ByteStr]) extends Message

case class GetBlock(signature: ByteStr) extends Message

case class RawBytes(code: Byte, data: Array[Byte]) extends Message
