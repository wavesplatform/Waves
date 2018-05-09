package scorex.transaction.smart.script

import com.wavesplatform.lang
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.Versioned
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.state.ByteStr
import monix.eval.Coeval
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.ScriptParseError

trait Script extends Versioned {
  val expr: version.ExprT
  val text: String
  val bytes: Coeval[ByteStr]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => version == that.version && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = version.value * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase58String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base58.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base58: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  object Expr {
    def unapply(arg: Script): Option[Terms.Typed.EXPR] = {
      if (arg.version == V1) Some(arg.expr.asInstanceOf[Terms.Typed.EXPR])
      else None
    }
  }
}
