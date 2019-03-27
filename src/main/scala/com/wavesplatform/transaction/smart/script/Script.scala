package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base64
import com.wavesplatform.lang.ContentType.{DApp, Expression}
import com.wavesplatform.lang.ScriptType.Account
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.transaction.ValidationError.ScriptParseError
import com.wavesplatform.utils.defaultDecompilerContext
import com.wavesplatform.transaction.smart.script.ContractScript.ContractScriptImpl
import monix.eval.Coeval
import com.wavesplatform.transaction.smart.script.v1.ExprScript
import DecompileInstances._
import shapeless.HList

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

  def fromBase64String(str: String, checkComplexity: Boolean = true): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.tryDecode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes, checkComplexity)
    } yield script

  type DirectiveMeta = List[(String, Any)]

  def decompile(s: Script): (String, DirectiveMeta) = {
    val (scriptText, directives: DirectiveMeta) = s match {
      case e: ExprScript =>
        val directives = HList(s.stdLibVersion, Expression).map(PolyDecompile).toList
        val decompiler = Decompiler(e.expr, defaultDecompilerContext)
        (decompiler, directives)
      case ContractScriptImpl(_, contract, _) =>
        val directives = HList(s.stdLibVersion, Account, DApp).map(PolyDecompile).toList
        val decompiler = Decompiler(contract, defaultDecompilerContext)
        (decompiler, directives)
    }
    val directivesText = directives
      .map { case (key, value) => s"{-#$key $value#-}" }
      .mkString(start = "", sep = "\n", end = "\n")

    (directivesText + scriptText, directives)
  }
}
