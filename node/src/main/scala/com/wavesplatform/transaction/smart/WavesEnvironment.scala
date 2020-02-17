package com.wavesplatform.transaction.smart

import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.MultiPaymentPolicyProvider._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.v1.traits.Environment.InputEntity
import com.wavesplatform.lang.v1.traits._
import com.wavesplatform.lang.v1.traits.domain.Recipient._
import com.wavesplatform.lang.v1.traits.domain._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.exchange.Order
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.eval.Coeval
import shapeless._

object WavesEnvironment {
  type In = Transaction :+: Order :+: PseudoTx :+: CNil
}

class WavesEnvironment(
    nByte: Byte,
    in: Coeval[Environment.InputEntity],
    h: Coeval[Int],
    blockchain: Blockchain,
    address: Coeval[ByteStr],
    ds: DirectiveSet,
    override val txId: ByteStr
) extends Environment[Id] {

  override def height: Long = h()

  override def multiPaymentAllowed: Boolean = blockchain.allowsMultiPayment

  override def transactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(tx => RealTransactionWrapper(tx, blockchain, ds.stdLibVersion, paymentTarget(ds, Some(ByteStr.empty))).explicitGet())

  override def inputEntity: InputEntity =
    in.value

  override def transferTransactionById(id: Array[Byte]): Option[Tx] =
    blockchain
      .transferById(id)
      .map(t => RealTransactionWrapper.mapTransferTx(t._2, ds.stdLibVersion))

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
        minSponsoredFee = Some(assetDesc.sponsorship).filter(_ != 0)
      )
    }
  }

  override def lastBlockOpt(): Option[BlockInfo] =
    blockchain.lastBlock
      .map(block => toBlockInfo(block.header, height.toInt, blockchain.hitSourceAtHeight(height.toInt)))

  override def blockInfoByHeight(blockHeight: Int): Option[BlockInfo] =
    blockchain.blockInfo(blockHeight)
      .map(blockHAndSize =>
        toBlockInfo(blockHAndSize.header, blockHeight, blockchain.hitSourceAtHeight(blockHeight)))

  private def toBlockInfo(blockH: BlockHeader, bHeight: Int, vrf: Option[ByteStr]) = {
    BlockInfo(
      timestamp = blockH.timestamp,
      height = bHeight,
      baseTarget = blockH.baseTarget,
      generationSignature = blockH.generationSignature,
      generator = blockH.generator.toAddress.bytes,
      generatorPublicKey = ByteStr(blockH.generator),
      if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5)) vrf else None
    )
  }
}
