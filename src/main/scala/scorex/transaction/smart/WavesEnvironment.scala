package scorex.transaction.smart

import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.state._
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.account.{Address, AddressOrAlias}
import scorex.transaction.Transaction
import com.wavesplatform.lang.v1.traits.{Transaction => ContractTransaction}

class WavesEnvironment(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Int = h()

  override def transaction: ContractTransaction = convert(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(convert)

  override def data(addressBytes: Array[Byte], key: String, dataType: DataType): Option[Any] = {
    val address = Address.fromBytes(addressBytes).explicitGet()
    val data    = blockchain.accountData(address, key)
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
      address <- blockchain.resolveAliasEi(aoa._1)
    } yield address.bytes.arr).left.map(_.toString)
  }

  def convert(tx: Transaction) = RealTransactionWrapper(tx)

  override def networkByte: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Array[Byte], maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa     <- AddressOrAlias.fromBytes(bytes = addressOrAlias, position = 0)
      address <- blockchain.resolveAliasEi(aoa._1)
      balance = blockchain.balance(address, maybeAssetId.map(ByteStr(_)))
    } yield balance).left.map(_.toString)
  }
  override def transactionHeightById(id: Array[Byte]): Option[Int] =
    blockchain.transactionHeight(ByteStr(id))
}
