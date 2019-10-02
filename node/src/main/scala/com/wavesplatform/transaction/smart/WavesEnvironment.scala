package com.wavesplatform.transaction.smart

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain.Recipient._
import com.wavesplatform.lang.v1.traits.domain.Tx.ScriptTransfer
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless._

object WavesEnvironment {
  type In = Transaction :+: Order :+: ScriptTransfer :+: CNil
}

class WavesEnvironment(nByte: Byte, in: Coeval[WavesEnvironment.In], h: Coeval[Int], blockchain: Blockchain, address: Coeval[ByteStr])
    extends Environment[Id] {
  override def height: Long = h()

  override def inputEntity: Environment.InputEntity = {
    in.apply()
      .map(InputPoly)
  }

  override def transactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(tx => RealTransactionWrapper(tx, Some(id)))

  override def transferTransactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transferById(id)
      .map(t => RealTransactionWrapper.mapTransferTx(t._2))

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

  override def assetInfoById(id: Array[Byte]): Option[domain.ScriptAssetInfo] = {
    blockchain.assetDescription(IssuedAsset(id)).map { assetDesc =>
      ScriptAssetInfo(
        id = id,
        quantity = assetDesc.totalVolume.toLong,
        decimals = assetDesc.decimals,
        issuer = Address(assetDesc.issuer.toAddress.bytes),
        issuerPk = assetDesc.issuer,
        reissuable = assetDesc.reissuable,
        scripted = assetDesc.script.nonEmpty,
        sponsored = assetDesc.sponsorship != 0
      )
    }
  }

  override def lastBlockOpt(): Option[BlockInfo] =
    blockchain.lastBlock.map(block => toBlockInfo(block.getHeader(), height.toInt))

  override def blockInfoByHeight(blockHeight: Int): Option[BlockInfo] =
    blockchain.blockHeaderAndSize(blockHeight).map(blockHAndSize => toBlockInfo(blockHAndSize._1, blockHeight))

  private def toBlockInfo(blockH: BlockHeader, bHeight: Int) = {
    BlockInfo(
      timestamp = blockH.timestamp,
      height = bHeight,
      baseTarget = blockH.consensusData.baseTarget,
      generationSignature = blockH.consensusData.generationSignature,
      generator = blockH.signerData.generator.toAddress.bytes,
      generatorPublicKey = ByteStr(blockH.signerData.generator)
    )
  }
}
