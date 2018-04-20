package scorex.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.ctx.impl.{CryptoContext, PureContext, WavesContext}
import com.wavesplatform.state._
import monix.eval.Coeval
import scorex.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.instance, CryptoContext.build(WavesCrypto))

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): Context =
    Monoid.combine(baseContext, WavesContext.build(new WavesEnvironment(nByte, tx, h, blockchain)))
}
