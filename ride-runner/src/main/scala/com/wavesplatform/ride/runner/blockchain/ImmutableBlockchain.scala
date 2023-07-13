package com.wavesplatform.ride.runner.blockchain

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.ride.runner.*
import com.wavesplatform.ride.runner.input.{RideRunnerInput, RunnerAccountState, RunnerScriptInfo}
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
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, ERC20Address, Proofs, Transaction, TxPositiveAmount}

class ImmutableBlockchain(override val settings: BlockchainSettings, input: RideRunnerInput) extends Blockchain {
  private val chainId: Byte = settings.addressSchemeCharacter.toByte

  private lazy val _hasData: Map[Address, Boolean] = mapAccountState(_.data.nonEmpty)

  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = _hasData.getOrElse(address, false)

  private lazy val _accountData: Map[Address, Map[String, DataEntry[?]]] = for {
    (addr, state) <- input.accounts
    data          <- state.data
  } yield addr -> data.map { case (key, entry) => key -> entry.toDataEntry(key) }

  private lazy val _accountScript: Map[Address, RunnerScriptInfo] = for {
    (addr, state) <- input.accounts
    scriptInfo    <- state.scriptInfo
  } yield addr -> scriptInfo

  // Ride: get*Value (data), get* (data)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def accountData(acc: Address, key: String): Option[DataEntry[?]] = _accountData.getOrElse(acc, Map.empty).get(key)

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = {
    _accountScript.get(address).map { input =>
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

  private lazy val _blockHeader: Map[Int, SignedBlockHeader] = for {
    (height, blockInfo) <- input.blocks
  } yield height -> SignedBlockHeader(
    header = BlockHeader(
      version = 5,
      timestamp = blockInfo.timestamp,
      reference = ByteStr(Array.emptyByteArray),
      baseTarget = blockInfo.baseTarget,
      generationSignature = blockInfo.generationSignature,
      generator = blockInfo.generatorPublicKey,
      featureVotes = Nil,
      rewardVote = -1,
      transactionsRoot = ByteStr(Array.emptyByteArray)
    ),
    signature = ByteStr(Array.emptyByteArray)
  )

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    // Dirty, but we have a clear error instead of "None.get"
    Some(
      _blockHeader.getOrElse(
        height,
        throw new RuntimeException(s"blockHeader($height): can't find a block header, please specify or check your script")
      )
    )

  private lazy val _hitSource: Map[Int, ByteStr] = for {
    (height, blockInfo) <- input.blocks
    vrf                 <- blockInfo.VRF
  } yield height -> vrf

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = _hitSource.get(height) // VRF

  // Ride: blockInfoByHeight
  override def blockReward(height: Int): Option[Long] = input.blocks.get(height).map(_.blockReward)

  // Ride: wavesBalance, height, lastBlock
  override def height: Int = input.height

  override val activatedFeatures: Map[Short, Int] =
    settings.functionalitySettings.preActivatedFeatures ++
      input.features.map(id => id -> (height - 1))

  private lazy val assets: Map[IssuedAsset, AssetDescription] = input.assets.map { case (asset, info) =>
    asset -> AssetDescription(
      originTransactionId = asset.id,
      issuer = info.issuerPublicKey,
      name = info.name,
      description = info.description,
      decimals = info.decimals,
      reissuable = info.reissuable,
      totalVolume = info.quantity,
      script = info.script.map { script =>
        val complexityInfo = complexityInfoOf(isAsset = true, script)
        AssetScriptInfo(script, complexityInfo.verifierComplexity)
      },
      sponsorship = info.minSponsoredAssetFee,
      // All next fields are not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
      lastUpdatedAt = Height(0),
      nft = false,
      sequenceInBlock = 0,
      issueHeight = Height(0)
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

  private lazy val _leaseBalance: Map[Address, LeaseBalance] = for {
    (addr, state) <- input.accounts
    lease         <- state.leasing
  } yield addr -> LeaseBalance(lease.in, lease.out)

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = _leaseBalance.getOrElse(address, LeaseBalance(0, 0))

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long =
    input.accounts.get(address).flatMap(_.balance(mayBeAssetId)).getOrElse(0L)

  private lazy val _balanceSnapshots: Map[Address, Seq[BalanceSnapshot]] = for {
    (addr, state) <- input.accounts
  } yield {
    val generatingBalance = state.generatingBalance.orElse(state.balance(Waves)).getOrElse(0L)
    addr -> Seq(BalanceSnapshot(height, generatingBalance, 0, 0))
  }

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    // "to" always None
    _balanceSnapshots.getOrElse(address, Seq(BalanceSnapshot(height, 0, 0, 0))).filter(_.height >= from)

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

  private lazy val transferById: Map[ByteStr, TransferTransactionLike] = for {
    (id, tx) <- input.transactions
  } yield id -> TransferTransaction(
    version = tx.version,
    sender = tx.senderPublicKey,
    recipient = tx.recipient,
    assetId = tx.assetId,
    amount = TxPositiveAmount.from(tx.amount).explicitGet(),
    feeAssetId = tx.feeAssetId,
    fee = TxPositiveAmount.from(tx.fee).explicitGet(),
    attachment = tx.attachment,
    timestamp = tx.timestamp,
    proofs = Proofs(tx.proofs),
    chainId = chainId
  )

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    transferById.get(id).map { tx =>
      val meta = transactionMeta(id).getOrElse(throw new RuntimeException(s"Can't find a metadata of the transaction $id"))
      (meta.height, tx)
    }

  private def mapAccountState[T](f: RunnerAccountState => T): Map[Address, T] = for {
    (addr, state) <- input.accounts
  } yield addr -> f(state)

  // Not supported

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

  override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

  override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

  override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")

  private def kill(methodName: String) = throw new RuntimeException(methodName)
}
