package com.wavesplatform.lang

import com.wavesplatform.lang.Evaluator.ExecResult
import scodec.bits.ByteVector

trait Domain {
  def height: ExecResult[Int]
  def id: ExecResult[ByteVector]
  def tpe: ExecResult[Int]
  def senderPk: ExecResult[ByteVector]
  def bodyBytes: ExecResult[ByteVector]
  def proof(i:Int): ExecResult[ByteVector]
}