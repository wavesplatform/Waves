package com.wavesplatform.network

import java.net.{InetAddress, InetSocketAddress}

import com.google.common.base.Charsets
import io.netty.buffer.ByteBuf

case class Handshake(
    applicationName: String,
    applicationVersion: (Int, Int, Int),
    nodeName: String,
    nodeNonce: Long,
    declaredAddress: Option[InetSocketAddress]) {
  def encode(out: ByteBuf): out.type = {
    out.writeByte(applicationName.length)
    out.writeBytes(applicationName.getBytes(Charsets.UTF_8))
    out.writeInt(applicationVersion._1)
    out.writeInt(applicationVersion._2)
    out.writeInt(applicationVersion._3)
    out.writeByte(nodeName.length)
    out.writeBytes(nodeName.getBytes(Charsets.UTF_8))
    out.writeLong(nodeNonce)
    declaredAddress match {
      case None => out.writeInt(0)
      case Some(addr) =>
        val addressBytes = addr.getAddress.getAddress
        out.writeInt(addressBytes.length + 4)
        out.writeBytes(addressBytes)
        out.writeInt(addr.getPort)
    }
    out.writeLong(System.currentTimeMillis() / 1000)

    out
  }
}

object Handshake {
  def decode(in: ByteBuf): Handshake = {
    val appNameSize = in.readByte()
    val appName = in.readSlice(appNameSize).toString(Charsets.UTF_8)
    val appVersion = (in.readInt(), in.readInt(), in.readInt())
    val nodeNameSize = in.readByte()
    val nodeName = in.readSlice(nodeNameSize).toString(Charsets.UTF_8)
    val nonce = in.readLong()
    val declaredAddressLength = in.readInt()
    // 0 for no declared address, 8 for ipv4 address + port, 20 for ipv6 address + port
    require(declaredAddressLength == 0 || declaredAddressLength == 8 || declaredAddressLength == 20,
      s"invalid declared address length: $declaredAddressLength")
    val isa = if (declaredAddressLength == 0) None else {
      val addressBytes = new Array[Byte](declaredAddressLength - 4)
      in.readBytes(addressBytes)
      val address = InetAddress.getByAddress(addressBytes)
      val port = in.readInt()
      Some(new InetSocketAddress(address, port))
    }
    in.readLong() // time is ignored

    Handshake(appName, appVersion, nodeName, nonce, isa)
  }
}
