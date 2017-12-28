package com.wavesplatform.matcher.util

import java.nio.ByteBuffer

import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state2.DataTypes.DTTemplate
import org.h2.mvstore.DataUtils.readVarLong
import org.h2.mvstore.WriteBuffer
import org.h2.mvstore.`type`.DataType

object MatcherDataTypes {
  val orderInfo: DataType = new DTTemplate {
    override def compare(a: scala.Any, b: scala.Any) = throw new UnsupportedOperationException

    override def read(buff: ByteBuffer) = {
      val v1 = readVarLong(buff)
      val v2 = readVarLong(buff)
      val v3 = buff.get() == 1
      OrderInfo(v1, v2, v3)
    }

    override def getMemory(obj: scala.Any) = 9 * 2 + 1

    override def write(buff: WriteBuffer, obj: scala.Any) = {
      val oi = obj.asInstanceOf[OrderInfo]
      buff.putVarLong(oi.amount)
      buff.putVarLong(oi.filled)
      buff.put(if (oi.canceled) 1.toByte else 0.toByte)
    }
  }

}
