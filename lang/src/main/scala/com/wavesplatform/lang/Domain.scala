package com.wavesplatform.lang

import com.wavesplatform.lang.Evaluator.ExecResult
import scodec.bits.ByteVector

trait Domain {
  def Height: ExecResult[Int]
  def Id: ExecResult[ByteVector]
  def Type: ExecResult[Int]
  def SenderPk: ExecResult[ByteVector]
  def BodyBytes: ExecResult[ByteVector]
  def Proof_0: ExecResult[ByteVector]
  def Proof_1: ExecResult[ByteVector]
  def Proof_2: ExecResult[ByteVector]
}