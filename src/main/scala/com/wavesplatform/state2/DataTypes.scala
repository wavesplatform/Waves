package com.wavesplatform.state2

import java.nio.ByteBuffer

import org.h2.mvstore.DataUtils._
import org.h2.mvstore.WriteBuffer
import org.h2.mvstore.`type`.DataType
import scorex.utils.ByteArray

object DataTypes {
  trait DTTemplate extends DataType {
    override def read(buff: ByteBuffer, obj: Array[AnyRef], len: Int, key: Boolean) =
      0 until len foreach { i => obj(i) = read(buff) }

    override def write(buff: WriteBuffer, obj: Array[AnyRef], len: Int, key: Boolean) =
      0 until len foreach { i => write(buff, obj(i)) }
  }

  val byteStr: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any): Int = ByteArray.compare(a.asInstanceOf[ByteStr].arr, b.asInstanceOf[ByteStr].arr)

    override def read(buff: ByteBuffer): AnyRef = {
      val len = readVarInt(buff)
      val dst = Array.fill(len)(0: Byte)
      buff.get(dst, 0, len)
      ByteStr(dst)
    }

    override def getMemory(obj: scala.Any): Int = 5 + obj.asInstanceOf[ByteStr].arr.length

    override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
      val byteStr = obj.asInstanceOf[ByteStr]
      buff.putVarInt(byteStr.arr.length).put(byteStr.arr)
    }
  }

  // (Boolean, Long)
  val assets: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Boolean, Long)]].compare(a.asInstanceOf[(Boolean, Long)], b.asInstanceOf[(Boolean, Long)])

    override def read(buff: ByteBuffer) = (buff.get() == 1, readVarLong(buff))

    override def getMemory(obj: scala.Any) = 10

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (b, l) = obj.asInstanceOf[(Boolean, Long)]
      buff.put(if (b) 1.toByte else 0.toByte)
      buff.putVarLong(l)
    }
  }

  // (Long, Long)
  val orderFills: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Long, Long)]].compare(a.asInstanceOf[(Long, Long)], b.asInstanceOf[(Long, Long)])

    override def read(buff: ByteBuffer) = (readVarLong(buff), readVarLong(buff))

    override def getMemory(obj: scala.Any) = 18

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (l1, l2) = obj.asInstanceOf[(Long, Long)]
      buff.putVarLong(l1)
      buff.putVarLong(l2)
    }
  }

  private type Portfolio = (Long, (Long, Long), Map[Array[Byte], Long])
  val portfolios: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val v1 = readVarLong(buff)
      val v2 = readVarLong(buff)
      val v3 = readVarLong(buff)
      val m = Seq.fill(readVarInt(buff)) {
        val length = readVarInt(buff)
        val bytes = new Array[Byte](length)
        buff.get(bytes)
        (bytes, readVarLong(buff))
      }.toMap

      (v1, (v2, v3), m)
    }

    override def getMemory(obj: scala.Any) = {
      val (_, _, m) = obj.asInstanceOf[(Long, (Long, Long), Map[Array[Byte], Long])]
      9 * 3 + 5 + m.keySet.map(_.length + 5 + 9).sum
    }

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (v1, (v2, v3), m) = obj.asInstanceOf[(Long, (Long, Long), Map[Array[Byte], Long])]
      buff.putVarLong(v1)
        .putVarLong(v2)
        .putVarLong(v3)
        .putVarInt(m.size)

      for ((k, v) <- m) {
        buff.putVarInt(k.length)
          .put(k)
          .putVarLong(v)
      }
    }
  }

  // (Int, Long, Long)
  val balanceSnapshots: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) =
      implicitly[Ordering[(Int, Long, Long)]].compare(a.asInstanceOf[(Int, Long, Long)], b.asInstanceOf[(Int, Long, Long)])

    override def read(buff: ByteBuffer) = (readVarInt(buff), readVarLong(buff), readVarLong(buff))

    override def getMemory(obj: scala.Any) = 23

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (v1, v2, v3) = obj.asInstanceOf[(Int, Long, Long)]
      buff.putVarInt(v1)
      buff.putVarLong(v2)
      buff.putVarLong(v3)
    }
  }

  // List[Array[Byte]]
  val accountTransactionIds: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = {
      val l1 = a.asInstanceOf[List[Array[Byte]]]
      val l2 = a.asInstanceOf[List[Array[Byte]]]
      if (l1.length != l2.length) l1.length - l2.length else {
        l1.zip(l2).view
          .map { case (b1, b2) => ByteArray.compare(b1, b2) }
          .dropWhile(_ == 0)
          .headOption.getOrElse(0)

      }
    }

    override def read(buff: ByteBuffer) = {
      val length = readVarInt(buff)
      List.fill(length) {
        val idLength = readVarInt(buff)
        val bytes = new Array[Byte](idLength)
        buff.get(bytes)
        bytes
      }
    }

    override def getMemory(obj: scala.Any) = 5 + obj.asInstanceOf[List[Array[Byte]]].map(_.length + 5).sum

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val ids = obj.asInstanceOf[List[Array[Byte]]]
      buff.putVarInt(ids.length)
      ids.foreach { b =>
        buff.putVarInt(b.length)
        buff.put(b)
      }
    }
  }

  // (Int, Array[Byte])
  val transactions: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val i = readVarInt(buff)
      val b = new Array[Byte](readVarInt(buff))
      buff.get(b)
      (i, b)
    }

    override def getMemory(obj: scala.Any) = 10 + obj.asInstanceOf[(Int, Array[Byte])]._2.length

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val (i, v) = obj.asInstanceOf[(Int, Array[Byte])]
      buff.putVarInt(i)
      buff.putVarInt(v.length)
      buff.put(v)
    }
  }
}
