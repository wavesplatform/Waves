package scorex.transaction.smart

import com.wavesplatform.lang.v1.traits.{DataType, Environment, Recipient, Tx => ContractTransaction}
import com.wavesplatform.state._
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.account.{Address, AddressOrAlias, Alias}
import scorex.transaction.Transaction

class WavesEnvironment(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Int = h()

  override def transaction: ContractTransaction = RealTransactionWrapper(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(RealTransactionWrapper(_))

  override def data(addressOrAlias: Array[Byte], key: String, dataType: DataType): Option[Any] = {
    (for {
      aoa     <- AddressOrAlias.fromBytes(addressOrAlias, 0)
      address <- blockchain.resolveAliasEi(aoa._1)
      data = blockchain
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (LongDataEntry(_, value), DataType.Long)        => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteVector(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data).fold(_ => None, identity)
  }
  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchain
      .resolveAliasEi(Alias.buildWithCurrentNetworkByte(name).explicitGet())
      .left
      .map(_.toString)
      .right
      .map(a => Recipient.Address(ByteVector(a.bytes.arr)))

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
