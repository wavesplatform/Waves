package com.wavesplatform.lang.utils

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.Serde.{FH_NATIVE, FH_USER}

object Serialize {
  implicit class ByteBufferOps(val self: ByteBuffer) extends AnyVal {
    def getBytes: Array[Byte] = {
      val len = self.getInt
      if (self.limit() < len || len < 0) {
        throw new Exception(s"Invalid array size ($len)")
      }
      val bytes = new Array[Byte](len)
      self.get(bytes)
      bytes
    }

    def getString: String = new String(getBytes, StandardCharsets.UTF_8)

    def getFunctionHeader: FunctionHeader = self.get() match {
      case FH_NATIVE => Native(self.getShort)
      case FH_USER   => User(getString)
      case x         => throw new RuntimeException(s"Unknown function header type: $x")
    }
  }

  implicit class ByteArrayOutputStreamOps(val self: ByteArrayOutputStream) extends AnyVal {
    def writeShort(value: Short): ByteArrayOutputStream = writeNumber(value, 2)
    def writeInt(value: Int): ByteArrayOutputStream     = writeNumber(value, 4)
    def writeLong(value: Long): ByteArrayOutputStream   = writeNumber(value, 8)

    def writeNumber(n: Long, byteCount: Int): ByteArrayOutputStream = {
      (byteCount - 1 to 0 by -1).foreach { i =>
        self.write((n >> (8 * i) & 0xffL).toInt)
      }
      self
    }

    def writeString(x: String): ByteArrayOutputStream = {
      val bytes = x.getBytes(StandardCharsets.UTF_8)
      self.writeInt(bytes.length)
      self.write(bytes)
      self
    }

    def writeFunctionHeader(h: FunctionHeader): ByteArrayOutputStream = h match {
      case FunctionHeader.Native(id) =>
        self.write(FH_NATIVE)
        self.writeShort(id)
      case FunctionHeader.User(internalName, _) =>
        self.write(FH_USER)
        self.writeString(internalName)
    }
  }
}
