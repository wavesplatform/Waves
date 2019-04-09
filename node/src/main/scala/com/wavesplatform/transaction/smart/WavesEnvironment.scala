package com.wavesplatform.transaction.smart

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.FairPoSCalculator
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain.Recipient._
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain.{BlockHeader, Recipient, Tx}
import com.wavesplatform.state._
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction, TransactionParsers}
import monix.eval.Coeval
import shapeless._

object WavesEnvironment {
  type In = Transaction :+: Order :+: ScriptTransfer :+: CNil
}

class WavesEnvironment(nByte: Byte, in: Coeval[WavesEnvironment.In], h: Coeval[Int], blockchain: Blockchain, address: Coeval[ByteStr])
    extends Environment {

  override def height: Long = h()

  override def inputEntity: Environment.InputEntity = {
    in.apply()
      .map(InputPoly)
  }

  override def transactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(RealTransactionWrapper(_))

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = {
    for {
      address <- recipient match {
        case Address(bytes) =>
          com.wavesplatform.account.Address
            .fromBytes(bytes.arr)
            .toOption
        case Alias(name) =>
          com.wavesplatform.account.Alias
            .create(name)
            .flatMap(blockchain.resolveAlias)
            .toOption
      }
      data <- blockchain
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (IntegerDataEntry(_, value), DataType.Long)     => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteStr(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data
  }
  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchain
      .resolveAlias(com.wavesplatform.account.Alias.create(name).explicitGet())
      .left
      .map(_.toString)
      .right
      .map(a => Recipient.Address(ByteStr(a.bytes.arr)))

  override def chainId: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = blockchain.balance(address, Asset.fromCompatId(maybeAssetId.map(ByteStr(_))))
    } yield balance).left.map(_.toString)
  }

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    blockchain.transactionHeight(ByteStr(id)).map(_.toLong)

  override def tthis: Address = Recipient.Address(address())

  override def transactionParser(bytes: Array[Byte]): Option[Tx] =
    TransactionParsers.parseBytes(bytes).toOption.map(RealTransactionWrapper(_))

  override def blockHeaderParser(bytes: Array[Byte]): Option[BlockHeader] =
    com.wavesplatform.block.BlockHeader
      .parseWithoutTransactions(bytes)
      .toOption
      .map { header =>
        BlockHeader(
          header.timestamp,
          header.version.toLong,
          header.reference,
          header.signerData.generator,
          header.signerData.signature,
          header.consensusData.baseTarget,
          header.consensusData.generationSignature,
          header.transactionCount,
          header.transactionTreeHash,
          header.minerWavesBalancesTreeHash,
          header.minerEffectiveBalancesTreeHash,
          header.featureVotes.map(_.toLong).toSeq
        )
      }

  override def calculatePoSDelay(hit: ByteStr, baseTarget: Long, balance: Long): Long = {
    FairPoSCalculator
      .calculateDelay(BigInt(1, hit.arr), baseTarget, balance)
  }

  override def accountScriptHash(addressOrAlias: Recipient): Option[Array[Byte]] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr, position = 0).map(_._1)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchain.resolveAlias(aoa)
      maybeScript = blockchain.accountScript(address).map { script =>
        com.wavesplatform.crypto.fastHash(script.bytes().arr)
      }
    } yield maybeScript).toOption.flatten
  }
}
