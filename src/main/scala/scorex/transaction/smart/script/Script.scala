package scorex.transaction.smart.script

import com.wavesplatform.lang.Versioned
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.ScriptParseError

trait Script extends Versioned {
  val expr: version.ExprT
  val complexity: Coeval[Long]
  val text: String
  val bytes: Coeval[ByteStr]
}

object Script {

  val checksumLength = 4

  def fromBase58String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base58.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base58: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

  implicit class ScriptOps(self: Script) {
    def extraFee(extraChargePerOperation: Double, state: SnapshotStateReader): Long =
      (self.complexity() * extraChargePerOperation).toLong
  }
}
