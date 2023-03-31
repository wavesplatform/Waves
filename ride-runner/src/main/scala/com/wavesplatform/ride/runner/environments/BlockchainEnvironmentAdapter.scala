package com.wavesplatform.ride.runner.environments

import cats.Id
import cats.syntax.either.*
import com.wavesplatform.account
import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.block.BlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.traits.Environment.{InputEntity, Tthis}
import com.wavesplatform.lang.v1.traits.domain.Recipient.{Address, Alias}
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.state.{BinaryDataEntry, Blockchain, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.{Asset, TransactionBase}
import monix.eval.Coeval

// See WavesEnvironment
// TODO modify WavesEnvironment?
class BlockchainEnvironmentAdapter(
    blockchainForScript: Blockchain,
    blockchainForRest: Blockchain,
    toTx: TransactionBase => Tx // realtransactionwrapper
) extends Environment[Id] {
  // Other functions need Blockchain
  override def height: Long                                 = blockchainForScript.height
  override def transactionById(id: Array[Byte]): Option[Tx] = blockchainForScript.transactionInfo(ByteStr(id)).map { case (_, tx) => toTx(tx) }
  override def transferTransactionById(id: Array[Byte]): Option[Tx.Transfer] =
    blockchainForScript
      .transferById(ByteStr(id))
      .map { case (_, tx) => toTx(tx) }
      .collect { case tx: Tx.Transfer => tx }

  override def transactionHeightById(id: Array[Byte]): Option[Long] =
    blockchainForScript.transactionMeta(ByteStr(id)).collect { case tm if tm.succeeded => tm.height.toLong }

  override def assetInfoById(id: Array[Byte]): Option[ScriptAssetInfo] =
    for {
      assetDesc <- blockchainForScript.assetDescription(IssuedAsset(ByteStr(id)))
    } yield {
      ScriptAssetInfo(
        id = ByteStr(id),
        name = assetDesc.name.toStringUtf8,
        description = assetDesc.description.toStringUtf8,
        quantity = assetDesc.totalVolume.toLong,
        decimals = assetDesc.decimals,
        issuer = Address(ByteStr(assetDesc.issuer.toAddress.bytes)),
        issuerPk = assetDesc.issuer,
        reissuable = assetDesc.reissuable,
        scripted = assetDesc.script.nonEmpty,
        minSponsoredFee = Some(assetDesc.sponsorship).filter(_ != 0)
      )
    }

  override def lastBlockOpt(): Option[BlockInfo] =
    blockchainForScript.lastBlockHeader
      .map(block => toBlockInfo(block.header, height.toInt, blockchainForScript.vrf(height.toInt)))

  private def toBlockInfo(blockH: BlockHeader, bHeight: Int, vrf: Option[ByteStr]) =
    // There are no new blocks in currentBlockchain
    BlockInfo(
      timestamp = blockH.timestamp,
      height = bHeight,
      baseTarget = blockH.baseTarget,
      generationSignature = blockH.generationSignature,
      generator = ByteStr(blockH.generator.toAddress.bytes),
      generatorPublicKey = blockH.generator,
      if (blockchainForRest.isFeatureActivated(BlockchainFeatures.BlockV5)) vrf else None
    )

  override def blockInfoByHeight(height: Int): Option[BlockInfo] =
    blockchainForScript
      .blockHeader(height)
      .map(blockHAndSize => toBlockInfo(blockHAndSize.header, height, blockchainForScript.vrf(height)))

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] =
    for {
      address <- toAddress(recipient)
      data <- blockchainForScript
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

  def toAddress(recipient: Recipient): Option[com.wavesplatform.account.Address] =
    recipient match {
      case Address(bytes) =>
        com.wavesplatform.account.Address
          .fromBytes(bytes.arr)
          .toOption
      case Alias(name) =>
        com.wavesplatform.account.Alias
          .create(name)
          .flatMap(blockchainForScript.resolveAlias)
          .toOption
    }

  override def hasData(recipient: Recipient): Boolean = {
    (for {
      address <- recipient match {
        case Address(bytes) =>
          com.wavesplatform.account.Address
            .fromBytes(bytes.arr, chainId)
            .toOption
        case Alias(name) =>
          com.wavesplatform.account.Alias
            .create(name)
            .flatMap(blockchainForScript.resolveAlias)
            .toOption
      }
    } yield blockchainForScript.hasData(address)).getOrElse(false)
  }

  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchainForScript
      .resolveAlias(com.wavesplatform.account.Alias.create(name).explicitGet())
      .map(a => Recipient.Address(ByteStr(a.bytes)))
      .left
      .map(_.toString)

  override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] =
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.arr)
        case Alias(name)    => com.wavesplatform.account.Alias.create(name)
      }
      address <- blockchainForScript.resolveAlias(aoa)
      balance = blockchainForScript.balance(address, Asset.fromCompatId(assetId.map(ByteStr(_))))
    } yield balance).left.map(_.toString)

  override def accountWavesBalanceOf(addressOrAlias: Recipient): Either[String, Environment.BalanceDetails] = {
    val addressE: Either[ValidationError, account.Address] = addressOrAlias match {
      case Address(bytes) => account.Address.fromBytes(bytes.arr)
      case Alias(name)    => account.Alias.create(name).flatMap(a => blockchainForScript.resolveAlias(a))
    }
    for {
      address <- addressE.leftMap(_.toString)
      portfolio = blockchainForScript.wavesPortfolio(address)
      effectiveBalance <- portfolio.effectiveBalance
    } yield Environment.BalanceDetails(
      portfolio.balance - portfolio.lease.out,
      portfolio.balance,
      blockchainForScript.generatingBalance(address),
      effectiveBalance
    )
  }

  override def accountScript(addressOrAlias: Recipient): Option[Script] =
    for {
      address <- toAddress(addressOrAlias)
      si      <- blockchainForScript.accountScript(address)
    } yield si.script

  override def callScript(
      dApp: Recipient.Address,
      func: String,
      args: List[Terms.EVALUATED],
      payments: Seq[(Option[Array[Byte]], Long)],
      availableComplexity: Int,
      reentrant: Boolean
  ): Coeval[(Either[ValidationError, (Terms.EVALUATED, Log[Id])], Int)] = ???

  // Constants
  override def chainId: Byte                = kill("chainId")
  override def inputEntity: InputEntity     = kill("inputEntity")
  override def tthis: Tthis                 = kill("tthis")
  override def multiPaymentAllowed: Boolean = kill("multiPaymentAllowed")
  override def txId: ByteStr                = kill("txId")

  // Functions those don't need Blockchain
  override def transferTransactionFromProto(b: Array[Byte]): Option[Tx.Transfer]           = kill(s"transferTransactionFromProto")
  override def addressFromString(address: String): Either[String, Recipient.Address]       = kill(s"addressFromString($address)")
  override def addressFromPublicKey(publicKey: ByteStr): Either[String, Recipient.Address] = kill(s"addressFromPublicKey($publicKey)")

  private def kill(methodName: String) = throw new RuntimeException(s"$methodName is not supported, contact with developers")
}
