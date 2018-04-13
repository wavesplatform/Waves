package scorex.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.v1.ctx.Context
import com.wavesplatform.lang.v1.ctx.impl.{CryptoContext, PureContext, WavesContext}
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction._

object BlockchainContext {

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader): Context =
    Monoid.combineAll(
      Seq(PureContext.instance, CryptoContext.build(WavesCrypto), WavesContext.build(new WavesEnvironment(nByte, tx, h, state), WavesCrypto)))
}
