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

  // (Long, Long, Long)
  val waves: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any): Int = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer): AnyRef = {
      val v1 = readVarLong(buff)
      val v2 = readVarLong(buff)
      val v3 = readVarLong(buff)
      (v1, v2, v3)
    }

    override def getMemory(obj: scala.Any): Int = 9 * 3 + 5

    override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
      val (v1, v2, v3) = obj.asInstanceOf[(Long, Long, Long)]
      buff.putVarLong(v1)
        .putVarLong(v2)
        .putVarLong(v3)
    }
  }

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

  //Map[Short,Int]
  val mapShortInt: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any): Int = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer): AnyRef = {
      val n = readVarInt(buff)
      val m = Seq.fill(n) {
        val feature = buff.getShort
        val state = buff.get()
        feature -> state.toInt
      }.toMap[Short, Int]
      m
    }

    override def getMemory(obj: scala.Any): Int = {
      val m = obj.asInstanceOf[Map[Short, Int]]
      5 + m.size * (2 + 4)
    }

    override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
      val m = obj.asInstanceOf[Map[Short, Int]]
      buff.putVarInt(m.size)
      for (v <- m) {
        buff.putShort(v._1)
        buff.putInt(v._2)
      }
    }
  }

  // List[ByteStr]
  val byteStrList: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val total = readVarInt(buff)
      Range(0, total).map { i =>
        val b = new Array[Byte](readVarInt(buff))
        buff.get(b)
        ByteStr(b)
      }.toList
    }


    override def getMemory(obj: scala.Any) = 5 + obj.asInstanceOf[List[ByteStr]].map(_.arr.length + 5).sum

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val list = obj.asInstanceOf[List[ByteStr]]
      buff.putVarInt(list.size)
      list.foreach { byteStr =>
        buff.putVarInt(byteStr.arr.length)
        buff.put(byteStr.arr)
      }
    }
  }
}
