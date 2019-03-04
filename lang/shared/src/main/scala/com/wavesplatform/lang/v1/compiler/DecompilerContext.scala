package com.wavesplatform.lang.v1.compiler

case class DecompilerContext(opCodes: Map[Short, String], binaryOps: Map[Short, String], ident: Int) {
  def incrementIdent(): DecompilerContext = this.copy(ident = this.ident + 1)
}
