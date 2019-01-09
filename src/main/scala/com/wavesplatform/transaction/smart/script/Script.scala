package com.wavesplatform.transaction.smart.script

import com.wavesplatform.lang.Versioned
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import com.wavesplatform.utils.Base64
import com.wavesplatform.transaction.ValidationError.ScriptParseError

trait Script extends Versioned {
  val expr: version.ExprT
  val text: String
  val bytes: Coeval[ByteStr]
  val complexity: Long

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => version == that.version && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = version.value * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase64String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  object Expr {
    def unapply(arg: Script): Option[Terms.EXPR] = arg.version.value match {
      case 1 | 2 => Some(arg.expr.asInstanceOf[Terms.EXPR])
      case _     => None
    }
  }
}
