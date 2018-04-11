package scorex.transaction.smart

import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.EvaluatorV1
import com.wavesplatform.lang.{ExecutionError, ScriptExpr, TypeInfo, v1}
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.account.AddressScheme
import scorex.transaction.Transaction

object ScriptRunner {

  def apply[A: TypeInfo, T <: Transaction](height: Int, tx: T, state: SnapshotStateReader, script: ScriptExpr): Either[ExecutionError, A] =
    script match {
      case V1(expr) => {
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          state
        )
        EvaluatorV1[A](ctx, expr.asInstanceOf[v1.Terms.Typed.EXPR])
      }

      case _ => Left("Unsupported script version")
    }
}
