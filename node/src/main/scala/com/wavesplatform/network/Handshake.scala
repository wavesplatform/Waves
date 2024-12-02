package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.base.Charsets
import io.netty.buffer.ByteBuf
import com.wavesplatform.utils.*

case class Handshake(
    applicationName: String,
    applicationVersion: (Int, Int, Int),
    nodeName: String,
    nodeNonce: Long,
    declaredAddress: Option[InetSocketAddress]
) {
  def encode(out: ByteBuf): out.type = {
    val applicationNameBytes = applicationName.utf8Bytes
    require(applicationNameBytes.length <= Byte.MaxValue, "The application name is too long!")
    out.writeByte(applicationNameBytes.length)
    out.writeBytes(applicationNameBytes)

    out.writeInt(applicationVersion._1)
    out.writeInt(applicationVersion._2)
    out.writeInt(applicationVersion._3)

    val nodeNameBytes = nodeName.utf8Bytes
    require(nodeNameBytes.length <= Byte.MaxValue, "A node name is too long!")
    out.writeByte(nodeNameBytes.length)
    out.writeBytes(nodeNameBytes)

    out.writeLong(nodeNonce)

    val peer = for {
      inetAddress <- declaredAddress
      address     <- Option(inetAddress.getAddress)
    } yield (address.getAddress, inetAddress.getPort)

    peer match {
      case None => out.writeInt(0)
      case Some((addressBytes, peerPort)) =>
        out.writeInt(addressBytes.length + Integer.BYTES)
        out.writeBytes(addressBytes)
        out.writeInt(peerPort)
    }

    out.writeLong(System.currentTimeMillis() / 1000)
    out
  }
}

object Handshake {
  class InvalidHandshakeException(msg: String) extends IllegalArgumentException(msg)

  def decode(in: ByteBuf): Handshake = {
    val appNameSize = in.readByte()

    if (appNameSize < 0 || appNameSize > Byte.MaxValue) {
      throw new InvalidHandshakeException(s"An invalid application name's size: $appNameSize")
    }
    val appName    = in.readSlice(appNameSize).toString(Charsets.UTF_8)
    val appVersion = (in.readInt(), in.readInt(), in.readInt())

    val nodeNameSize = in.readByte()
    if (nodeNameSize < 0 || nodeNameSize > Byte.MaxValue) {
      throw new InvalidHandshakeException(s"An invalid node name's size: $nodeNameSize")
    }
    val nodeName = in.readSlice(nodeNameSize).toString(Charsets.UTF_8)

    val nonce = in.readLong()

    val declaredAddressLength = in.readInt()
    // 0 for no declared address, 8 for ipv4 address + port, 20 for ipv6 address + port
    if (declaredAddressLength != 0 && declaredAddressLength != 8 && declaredAddressLength != 20) {
      throw new InvalidHandshakeException(s"An invalid declared address length: $declaredAddressLength")
    }
    val isa =
      if (declaredAddressLength == 0) None
      else {
        val addressBytes = new Array[Byte](declaredAddressLength - Integer.BYTES)
        in.readBytes(addressBytes)
        val address = InetAddress.getByAddress(addressBytes)
        val port    = in.readInt()
        Some(new InetSocketAddress(address, port))
      }
    in.readLong() // time is ignored

    Handshake(appName, appVersion, nodeName, nonce, isa)
  }
}
