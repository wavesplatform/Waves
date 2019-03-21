package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.transaction.ValidationError.ScriptParseError
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import monix.eval.Coeval

trait Script {
  type Expr

  val stdLibVersion: StdLibVersion

  val expr: Expr

  val bytes: Coeval[ByteStr]
  val complexity: Long

  val containsBlockV2: Coeval[Boolean]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => stdLibVersion == that.stdLibVersion && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = stdLibVersion * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase64String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.tryDecode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  def decompile(s: Script): String = s match {
    case e: ExprScript => Decompiler(e.expr, com.wavesplatform.utils.defaultDecompilerContext)
    case com.wavesplatform.transaction.smart.script.ContractScript.ContractScriptImpl(_, contract, _) =>
      Decompiler(contract, com.wavesplatform.utils.defaultDecompilerContext)
  }
}
