package com.wavesplatform.lang.v1.compiler

case class DecompilerContext(opCodes: Map[Short, String], binaryOps: Map[Short, String], ident: Int) {
  def inc(): DecompilerContext = this.copy(ident = this.ident + 1)
  def zero(): DecompilerContext = this.copy(ident = 0)
}
