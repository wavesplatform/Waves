package com.wavesplatform.lang
import com.wavesplatform.lang.Evaluator.ExecResult
import scodec.bits.ByteVector

class HeightDomain(h: Int) extends Domain {
  override def height: ExecResult[Int]               = Right(h)
  override def id: ExecResult[ByteVector]            = ???
  override def tpe: ExecResult[Int]                  = ???
  override def senderPk: ExecResult[ByteVector]      = ???
  override def bodyBytes: ExecResult[ByteVector]     = ???
  override def proof(i: Int): ExecResult[ByteVector] = ???
}
