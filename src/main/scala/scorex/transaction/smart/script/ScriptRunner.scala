package scorex.transaction.smart.script

import com.wavesplatform.lang.v1.EvaluatorV1
import com.wavesplatform.lang.{ExecutionError, TypeInfo}
import com.wavesplatform.state._
import monix.eval.Coeval
import scorex.account.AddressScheme
import scorex.transaction.Transaction
import scorex.transaction.smart.BlockchainContext
import scorex.transaction.smart.script.v1.ScriptV1
import cats.implicits._

object ScriptRunner {

  def apply[A: TypeInfo, T <: Transaction](height: Int, tx: T, blockchain: Blockchain, script: Script): Either[ExecutionError, A] =
    script match {
      case ScriptV1(expr) => {
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1[A](ctx, expr).left.map(_._3)
      }

      case _ => "Unsupported script version".asLeft[A]
    }

}
