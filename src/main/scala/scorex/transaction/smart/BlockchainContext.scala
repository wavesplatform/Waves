package scorex.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.state._
import monix.eval.Coeval
import scorex.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.evalContext, CryptoContext.evalContext(Global))

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, WavesContext.evalContext(new WavesEnvironment(nByte, tx, h, blockchain)))
}
