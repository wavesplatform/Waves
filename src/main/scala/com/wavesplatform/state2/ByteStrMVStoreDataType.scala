package com.wavesplatform.state2

import java.nio.ByteBuffer

import org.h2.mvstore.`type`.DataType
import org.h2.mvstore.{DataUtils, WriteBuffer}

/*
    https://github.com/h2database/h2database/blob/master/h2/src/main/org/h2/mvstore/type/StringDataType.java
*/
class ByteStrMVStoreDataType extends DataType {

  override def compare(a: scala.Any, b: scala.Any): Int = a.asInstanceOf[ByteStr].hashCode() compareTo b.asInstanceOf[ByteStr].hashCode()

  override def read(buff: ByteBuffer): AnyRef = {
    val len = DataUtils.readVarInt(buff)
    val dst = Array.fill(len){0:Byte}
    buff.get(dst, 0, len)
    ByteStr(dst)
  }

  override def read(buff: ByteBuffer, obj: Array[AnyRef], len: Int, key: Boolean): Unit =
    Range(0, len).foreach { i =>
      obj(i) = read(buff)
    }

  override def getMemory(obj: scala.Any): Int = 5 + obj.asInstanceOf[ByteStr].arr.length

  override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
    val eba = obj.asInstanceOf[ByteStr]
    buff.putVarInt(eba.arr.length).put(eba.arr)
  }

  override def write(buff: WriteBuffer, obj: Array[AnyRef], len: Int, key: Boolean): Unit =
    Range(0, len).foreach { i =>
      write(buff, obj(i))
    }
}
