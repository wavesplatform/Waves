package com.wavesplatform

import com.google.common.io.{ByteArrayDataInput, ByteArrayDataOutput}
import com.wavesplatform.state2._
import scorex.transaction.smart.Script

package object database {
  implicit class ByteArrayDataOutputExt(val output: ByteArrayDataOutput) extends AnyVal {
    def writeBigInt(v: BigInt): Unit = {
      val b = v.toByteArray
      val len = b.length
      output.writeByte(len)
      output.write(b)
    }

    def writeScriptOption(v: Option[Script]): Unit = {
      output.writeBoolean(v.isDefined)
      v.foreach { s =>
        val b = s.bytes().arr
        output.writeShort(b.length)
        output.write(b)
      }
    }
  }

  implicit class ByteArrayDataInputExt(val input: ByteArrayDataInput) extends AnyVal {
    def readBigInt(): BigInt = {
      val len = input.readByte()
      val b = new Array[Byte](len)
      input.readFully(b)
      BigInt(b)
    }

    def readScriptOption(): Option[Script] = {
      if (input.readBoolean()) {
        val len = input.readShort()
        val b = new Array[Byte](len)
        input.readFully(b)
        Some(Script.fromBytes(b).explicitGet())
      } else None
    }
  }
}
