package scorex.transaction.smart

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.transaction.ValidationError.ScriptParseError

case class Script() {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr.empty)
  val text: String = "foo"
}

object Script {
  def fromBytes(arr: Array[Byte]): Either[ScriptParseError, Script] = Right(Script())
}
