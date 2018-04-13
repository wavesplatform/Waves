package scorex.transaction.smart.script

import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.Terms.Typed
import com.wavesplatform.lang.{ScriptVersion, Versioned}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.ScriptParseError
import scorex.transaction.smart.script.v1.ScriptV1

trait Script extends Versioned {
  val expr: version.ExprT
  val text: String
  val bytes: Coeval[ByteStr]
}

object Script {

  val checksumLength = 4

  def apply[V <: ScriptVersion](v: V)(expr: v.ExprT): Script = {
    v match {
      case V1 => ScriptV1(expr.asInstanceOf[Typed.EXPR])
    }
  }

  def fromBase58String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base58.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base58: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  final case class ScriptVersionOps[V <: ScriptVersion](v: V) extends AnyVal {
    def unapply(arg: Script): Option[v.ExprT] = {
      if (arg.version == v) Some(arg.expr.asInstanceOf[v.ExprT])
      else None
    }
  }

  implicit def toOps[V <: ScriptVersion](v: V): ScriptVersionOps[V] = ScriptVersionOps(v)
}
