package com.wavesplatform.it.network.client

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
        out.writeBytes(addr.getAddress.getAddress)
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
    val fas = in.readInt()
    val isa = if (fas <= 0) None else {
      val addressBytes = new Array[Byte](4)
      in.readBytes(addressBytes)
      val address = InetAddress.getByAddress(addressBytes)
      val port = in.readInt()
      Some(new InetSocketAddress(address, port))
    }
    in.readLong() // time is ignored

    Handshake(appName, appVersion, nodeName, nonce, isa)
  }
}
