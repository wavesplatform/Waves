package com.wavesplatform.lang.utils

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.Serde.{FH_NATIVE, FH_USER}

import scala.annotation.tailrec

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

    def writeSignedVarInt(x: Int): ByteArrayOutputStream = {
      writeUnsignedVarInt((x << 1) ^ (x >> 31))
    }

    def writeUnsignedVarInt(v: Int): ByteArrayOutputStream = {
      @tailrec
      def rec(v: Int, acc: List[Byte]): List[Byte] = {
        if ((v & 0xFFFFF80) == 0L) {
          ((v & 0x7F).toByte :: acc).reverse
        } else {
          rec(v >>> 7, ((v & 0x7F) | 0x80).toByte :: acc)
        }
      }

      val bytes = rec(v, List.empty)
      bytes.foreach(self.write(_))
      self
    }

    def writeSignedVarLong(x: Long): ByteArrayOutputStream = {
      writeUnsignedVarLong((x << 1) ^ (x >> 63))
    }

    def writeUnsignedVarLong(v: Long): ByteArrayOutputStream = {
      @tailrec
      def rec(v: Long, acc: List[Byte]): List[Byte] = {
        if ((v & 0xFFFFFFFFFFFFFF80L) == 0L) {
          ((v & 0x7F).toByte :: acc).reverse
        } else {
          rec(v >>> 7, ((v & 0x7F) | 0x80).toByte :: acc)
        }
      }

      val bytes = rec(v, List.empty)
      bytes.foreach(self.write(_))
      self
    }

    def writeString(x: String): ByteArrayOutputStream = {
      val bytes = x.getBytes(StandardCharsets.UTF_8)
      self.writeInt(bytes.length)
      self.write(bytes)
      self
    }

    def writeStringOptimized(x: String): ByteArrayOutputStream = {
      val bytes = x.getBytes(StandardCharsets.UTF_8)
      self.writeUnsignedVarInt(bytes.length)
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

    def writeFunctionHeaderOptimized(h: FunctionHeader): ByteArrayOutputStream = h match {
      case FunctionHeader.Native(id) =>
        self.write(FH_NATIVE)
        self.writeUnsignedVarInt(id)
      case FunctionHeader.User(internalName, _) =>
        self.write(FH_USER)
        self.writeStringOptimized(internalName)
    }
  }
}
