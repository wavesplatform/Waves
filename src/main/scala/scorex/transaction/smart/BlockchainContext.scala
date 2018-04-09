package scorex.transaction.smart

import cats.kernel.Monoid
import com.wavesplatform.lang.BaseGlobal
import com.wavesplatform.lang.ctx.Context
import com.wavesplatform.lang.ctx.impl.{CryptoContext, PureContext, WavesContext}
import com.wavesplatform.lang.traits.{DataType, Environment, Transaction => ContractTransaction}
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.account.{Address, AddressOrAlias}
import scorex.transaction._

object BlockchainContext {
  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], state: SnapshotStateReader): Context = {
    val env = new Environment {
      override def height: Int = h()

      override def transaction: ContractTransaction = convert(tx())

      override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
        state
          .transactionInfo(ByteStr(id))
          .map(_._2)
          .map(convert)

      override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = {
        val address = Address.fromBytes(addressBytes).explicitGet()
        val data    = state.accountData(address, key)
        data.map((_, dataType)).flatMap {
          case (LongDataEntry(_, value), DataType.Long)        => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteVector(value.arr))
          case _                                               => None
        }
      }

      override def resolveAddress(addressOrAlias: Array[Byte]): Either[String, Array[Byte]] = {
        (for {
          aoa     <- AddressOrAlias.fromBytes(bytes = addressOrAlias, position = 0)
          address <- state.resolveAliasEi(aoa._1)
        } yield address.bytes.arr).left.map(_.toString)
      }

      def convert(tx: Transaction) = RealTransactionWrapper(tx)

      override def networkByte: Byte = nByte
    }
    val global: BaseGlobal = ???

    Monoid.combineAll(Seq(PureContext.instance, CryptoContext.build(global), WavesContext.build(env, global)))
  }
}
