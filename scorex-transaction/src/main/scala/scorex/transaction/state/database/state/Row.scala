package scorex.transaction.state.database.state

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Longs}
import org.h2.mvstore.WriteBuffer
import org.h2.mvstore.`type`.DataType
import scorex.serialization.BytesSerializable
import scorex.transaction.{FeesStateChange, LagonakiTransaction, StateChangeReason}

@SerialVersionUID(-3499112732510272830L)
case class Row(state: AccState, reason: Reason, lastRowHeight: Int) extends DataType with BytesSerializable {

  lazy val bytes: Array[Byte] = Ints.toByteArray(lastRowHeight) ++
    Longs.toByteArray(state.balance) ++
    Ints.toByteArray(reason.length) ++
    reason.foldLeft(Array.empty: Array[Byte]) { (b, scr) =>
      b ++ Ints.toByteArray(scr.bytes.length) ++ scr.bytes
    }

  override def compare(a: scala.Any, b: scala.Any): Int = (a, b) match {
    case (o1: Row, o2: Row) => BigInt(o1.bytes).compare(BigInt(o2.bytes))
    case _ => 1
  }

  override def write(buff: WriteBuffer, obj: scala.Any): Unit = {
    buff.put(obj.asInstanceOf[Row].bytes)
  }

  override def write(buff: WriteBuffer, obj: Array[AnyRef], len: Int, key: Boolean): Unit =
    obj.foreach(o => write(buff, o))

  override def read(buff: ByteBuffer): AnyRef = Row.deserialize(buff)

  override def read(buff: ByteBuffer, obj: Array[AnyRef], len: Int, key: Boolean): Unit = {
    (0 until len) foreach { i =>
      obj(i) = read(buff);
    }
  }

  override def getMemory(obj: scala.Any): Int = bytes.length
}

object Row {
  def deserialize(bytes: Array[Byte]): Row = {
    val b = ByteBuffer.allocate(bytes.length)
    b.put(bytes)
    b.flip()
    deserialize(b)
  }


  def deserialize(b: ByteBuffer): Row = {
    val lrh = b.getInt
    val accBalance = b.getLong
    val reasonLength = b.getInt
    val reason: Seq[StateChangeReason] = (0 until reasonLength) map { i =>
      val txSize = b.getInt
      val tx = new Array[Byte](txSize)
      b.get(tx)
      if (txSize == 8) FeesStateChange(Longs.fromByteArray(tx))
      else LagonakiTransaction.parseBytes(tx).get //todo: .get w/out catching
    }
    Row(AccState(accBalance), reason.toList, lrh)
  }
}