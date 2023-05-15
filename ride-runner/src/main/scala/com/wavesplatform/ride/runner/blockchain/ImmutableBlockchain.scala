package com.wavesplatform.ride.runner.blockchain

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.ride.runner.*
import com.wavesplatform.ride.runner.input.RideRunnerInput
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  Blockchain,
  DataEntry,
  Height,
  LeaseBalance,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, ERC20Address, Proofs, Transaction, TxPositiveAmount}

import java.nio.charset.StandardCharsets
import scala.util.chaining.scalaUtilChainingOps

class ImmutableBlockchain(override val settings: BlockchainSettings, input: RideRunnerInput) extends Blockchain {
  private val chainId: Byte = settings.addressSchemeCharacter.toByte

  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = input.hasData.getOrElse(address, false)

  // Ride: get*Value (data), get* (data)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def accountData(acc: Address, key: String): Option[DataEntry[?]] =
    input.accountData.getOrElse(acc, Map.empty).get(key)

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = {
    input.accountScript.get(address).map { input =>
      val complexityInfo = Set(ScriptEstimatorV1, ScriptEstimatorV2, this.estimator).map { estimator =>
        estimator.version -> complexityInfoOf(isAsset = false, input.script)
      }

      val (lastEstimatorVersion, lastComplexityInfo) = complexityInfo.last
      AccountScriptInfo(
        script = input.script,
        publicKey = input.publicKey,
        verifierComplexity = lastComplexityInfo.verifierComplexity,
        complexitiesByEstimator = complexityInfo
          .map { case (v, complexityInfo) => v -> complexityInfo.callableComplexities }
          .toMap
          .updated(lastEstimatorVersion, lastComplexityInfo.callableComplexities) // to preserve
      )
    }
  }

  // Indirectly
  override def hasAccountScript(address: Address): Boolean = accountScript(address).nonEmpty

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    // Dirty, but we have a clear error instead of "None.get"
    Some(
      input.blockHeader.getOrElse(
        height,
        throw new RuntimeException(s"blockHeader($height): can't find a block header, please specify or check your script")
      )
    )

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = input.hitSource.get(height) // VRF

  // Ride: wavesBalance, height, lastBlock
  override def height: Int = input.height

  override val activatedFeatures: Map[Short, Int] =
    settings.functionalitySettings.preActivatedFeatures ++
      input.extraFeatures.map(id => id -> (height - 1))

  private lazy val assets: Map[IssuedAsset, AssetDescription] = input.assets.map { case (asset, info) =>
    asset -> AssetDescription(
      originTransactionId = asset.id,
      issuer = info.issuerPublicKey,
      name = UnsafeByteOperations.unsafeWrap(info.name.getBytes(StandardCharsets.UTF_8)),
      description = UnsafeByteOperations.unsafeWrap(info.description.getBytes(StandardCharsets.UTF_8)),
      decimals = info.decimals,
      reissuable = info.reissuable,
      totalVolume = info.quantity,
      script = info.script.map { script =>
        val complexityInfo = complexityInfoOf(isAsset = true, script)
        AssetScriptInfo(script, complexityInfo.verifierComplexity)
      },
      sponsorship = info.minSponsoredAssetFee,
      // All next fields are not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
      lastUpdatedAt = Height @@ 0,
      nft = false,
      sequenceInBlock = 0,
      issueHeight = Height @@ 0
    )
  }

  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = assets.get(id)

  // Ride (indirectly): asset script validation
  override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assets.get(id).flatMap(_.script)

  private lazy val resolveAlias: Map[Alias, Address] = for {
    (addr, state) <- input.accounts
    alias         <- state.aliases
  } yield alias -> addr

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] =
    resolveAlias.get(a).toRight(AliasDoesNotExist(a): ValidationError)

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = input.leaseBalance.getOrElse(address, LeaseBalance(0, 0))

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long =
    input.balance.get(address).flatMap(_.get(mayBeAssetId)).getOrElse(0)

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    // "to" always None
    input.balanceSnapshots.getOrElse(address, Seq(BalanceSnapshot(height, 0, 0, 0))).filter(_.height >= from)

  private def complexityInfoOf(isAsset: Boolean, script: Script): ComplexityInfo =
    estimate(height, activatedFeatures, this.estimator, script, isAsset = isAsset, withCombinedContext = true)

  private lazy val transactionMetaById: Map[ByteStr, TxMeta] = for {
    (id, tx) <- input.transactions
  } yield id -> TxMeta(
    height = Height(tx.height.getOrElse((height - 1).max(1))),
    succeeded = true,
    spentComplexity = 0
  )

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] = transactionMetaById.get(id)

  private val emptyAddress = Address(new Array[Byte](Address.HashLength), chainId)

  private lazy val transferById: Map[ByteStr, TransferTransactionLike] = for {
    (id, tx) <- input.transactions
  } yield id -> TransferTransaction(
    version = tx.version,
    sender = tx.senderPublicKey,
    recipient = tx.recipient
      .map { x =>
        if (x.startsWith("alias:"))
          Alias.fromString(x).explicitGet().tap { x =>
            require(
              x.chainId == settings.addressSchemeCharacter,
              s"Expected for alias '$x' to be from '${settings.addressSchemeCharacter}' network"
            )
          }
        else Address.fromString(x).left.map(_ => Alias.createWithChainId(x, chainId).explicitGet()).merge
      }
      .getOrElse(emptyAddress),
    assetId = tx.assetId,
    amount = TxPositiveAmount.from(tx.amount).explicitGet(),
    feeAssetId = tx.feeAssetId,
    fee = TxPositiveAmount.from(tx.fee).explicitGet(),
    attachment = ByteStr(tx.attachment.getBytes(StandardCharsets.UTF_8)), // TODO
    timestamp = tx.timestamp,
    proofs = Proofs(tx.proofs.map(x => ByteStr(x.getBytes(StandardCharsets.UTF_8)))), // TODO
    chainId = chainId
  )

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    transferById.get(id).map { tx =>
      val meta = transactionMeta(id).getOrElse(throw new RuntimeException(s"Can't find a metadata of the transaction $id"))
      (meta.height, tx)
    }

  override def score: BigInt = kill("score")

  override def carryFee: Long = kill("carryFee")

  override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

  /** Features related */
  override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

  override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

  override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

  override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

  override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

  override def transactionInfo(id: BlockId): Option[(TxMeta, Transaction)] = kill("transactionInfo")

  /** Block reward related */
  override def blockReward(height: Int): Option[Long] = kill("blockReward")

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")

  private def kill(methodName: String) = throw new RuntimeException(methodName)
}