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

  val sigVerify: BOOL = SIGVERIFY(Accessor(TX, Field.BodyBytes), Accessor(TX, Field.Proof_0), Accessor(TX, Field.SenderPk))

  val multisig2Of3 = EQINT(
    SUM(
      SUM(
        IF(SIGVERIFY(Accessor(TX, Field.BodyBytes), Accessor(TX, Field.Proof_0), ???), CONST(1), CONST(0)),
        IF(SIGVERIFY(Accessor(TX, Field.BodyBytes), Accessor(TX, Field.Proof_1), ???), CONST(1), CONST(0))
      ),
      IF(SIGVERIFY(Accessor(TX, Field.BodyBytes), Accessor(TX, Field.Proof_2), ???), CONST(1), CONST(0))
    ), CONST(2))
}
