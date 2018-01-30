package com.wavesplatform.lang
import com.wavesplatform.lang.Evaluator.ExecResult
import scodec.bits.ByteVector

class HeightDomain(h: Int) extends Domain {
  override def Height: ExecResult[Int]           = Right(h)
  override def Id: ExecResult[ByteVector]        = ???
  override def Type: ExecResult[Int]             = ???
  override def SenderPk: ExecResult[ByteVector]  = ???
  override def BodyBytes: ExecResult[ByteVector] = ???
  override def Proof_0: ExecResult[ByteVector]   = ???
  override def Proof_1: ExecResult[ByteVector]   = ???
  override def Proof_2: ExecResult[ByteVector]   = ???
}
