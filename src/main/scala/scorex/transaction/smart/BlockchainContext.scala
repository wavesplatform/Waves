package scorex.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.ctx.impl.{CryptoContext, PureContext, WavesContext}
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.instance, CryptoContext.build(Global))

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader): Context =
    Monoid.combine(baseContext, WavesContext.build(new WavesEnvironment(nByte, tx, h, state)))
}
