package scorex.transaction.smart

import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import scorex.transaction.ValidationError.ScriptParseError
import scorex.transaction.smart.lang.Terms._

case class Script(script: BOOL) {
  val bytes: Coeval[ByteStr] = Coeval.evalOnce(ByteStr.empty)
  val version = 1
  val text: String = script.toString
}

object Script {
  def fromBytes(arr: Array[Byte]): Either[ScriptParseError, Script] = Right(Script())

  def apply(): Script = new Script(sigVerify)

  val sigVerify: BOOL = SIG_VERIFY(Accessor(TX, BodyBytes), Accessor(TX, Proof_0), Accessor(TX, SenderPk))

  val multisig2Of3 = EQ_INT(
    SUM(
      SUM(
        IF(SIG_VERIFY(Accessor(TX, BodyBytes), Accessor(TX, Proof_0), ???), CONST_INT(1), CONST_INT(0)),
        IF(SIG_VERIFY(Accessor(TX, BodyBytes), Accessor(TX, Proof_1), ???), CONST_INT(1), CONST_INT(0))
      ),
      IF(SIG_VERIFY(Accessor(TX, BodyBytes), Accessor(TX, Proof_2), ???), CONST_INT(1), CONST_INT(0))
    ), CONST_INT(2))
}
