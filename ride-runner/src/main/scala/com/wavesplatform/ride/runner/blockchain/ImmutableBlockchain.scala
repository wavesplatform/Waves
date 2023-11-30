package com.wavesplatform.ride.runner.blockchain

import com.github.benmanes.caffeine.cache.{CacheLoader, Caffeine, LoadingCache}
import com.google.protobuf.UnsafeByteOperations
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
import com.wavesplatform.ride.runner.input.RideRunnerBlockchainState
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, BalanceSnapshot, DataEntry, Height, LeaseBalance, TxMeta}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.AliasDoesNotExist
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, Proofs, Transaction, TxPositiveAmount}

import scala.util.chaining.scalaUtilChainingOps

class ImmutableBlockchain(override val settings: BlockchainSettings, input: RideRunnerBlockchainState) extends SupportedBlockchain { blockchain =>
  private val chainId: Byte = settings.addressSchemeCharacter.toByte

  // Ride: isDataStorageUntouched
  override def hasData(address: Address): Boolean = input.accounts.get(address).fold(false)(_.data.nonEmpty)

  // Ride: get*Value (data), get* (data)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def accountData(acc: Address, key: String): Option[DataEntry[?]] = for {
    accountState <- input.accounts.get(acc)
    data         <- accountState.data
    entry        <- data.get(key)
  } yield entry.toDataEntry(key)

  private val accountScripts = mkCache[Address, Option[AccountScriptInfo]] { address =>
    for {
      accountState <- input.accounts.get(address)
      scriptInfo   <- accountState.scriptInfo
    } yield {
      val complexityInfo = Set(ScriptEstimatorV1, ScriptEstimatorV2, blockchain.estimator).map { estimator =>
        estimator.version -> complexityInfoOf(isAsset = false, scriptInfo.script)
      }

      val (lastEstimatorVersion, lastComplexityInfo) = complexityInfo.last
      AccountScriptInfo(
        script = scriptInfo.script,
        publicKey = scriptInfo.publicKey,
        verifierComplexity = lastComplexityInfo.verifierComplexity,
        complexitiesByEstimator = complexityInfo
          .map { case (v, complexityInfo) => v -> complexityInfo.callableComplexities }
          .toMap
          .updated(lastEstimatorVersion, lastComplexityInfo.callableComplexities) // to preserve
      )
    }
  }

  // Ride: scriptHash
  override def accountScript(address: Address): Option[AccountScriptInfo] = accountScripts.get(address)

  private val blockHeaders = mkCache[Int, Option[SignedBlockHeader]] { height =>
    input.blocks.get(height).map { blockInfo =>
      SignedBlockHeader(
        header = BlockHeader(
          version = 5,
          timestamp = blockInfo.timestamp,
          reference = ByteStr(Array.emptyByteArray),
          baseTarget = blockInfo.baseTarget,
          generationSignature = blockInfo.generationSignature,
          generator = blockInfo.generatorPublicKey,
          featureVotes = Nil,
          rewardVote = -1,
          transactionsRoot = ByteStr(Array.emptyByteArray),
          None,
          None
        ),
        signature = ByteStr(Array.emptyByteArray)
      )
    }
  }

  // Ride: blockInfoByHeight, lastBlock
  override def blockHeader(height: Int): Option[SignedBlockHeader] =
    // Dirty, but we have a clear error instead of "None.get"
    blockHeaders
      .get(height)
      .tap { r =>
        if (r.isEmpty) throw new RuntimeException(s"blockHeader($height): can't find a block header, please specify or check your script")
      }

  // Ride: blockInfoByHeight
  override def hitSource(height: Int): Option[ByteStr] = input.blocks.get(height).flatMap(_.VRF)

  // Ride: blockInfoByHeight
  override def blockReward(height: Int): Option[Long] = input.blocks.get(height).map(_.blockReward)

  // Ride: wavesBalance, height, lastBlock
  override def height: Int = input.height

  override val activatedFeatures: ActivatedFeatures = settings.functionalitySettings.preActivatedFeatures ++ input.features.map(id => id -> height)

  private val assets = mkCache[IssuedAsset, Option[AssetDescription]] { assetId =>
    input.assets.get(assetId).map { info =>
      AssetDescription(
        originTransactionId = assetId.id,
        issuer = info.issuerPublicKey,
        name = UnsafeByteOperations.unsafeWrap(info.name),
        description = UnsafeByteOperations.unsafeWrap(info.description),
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
  }

  // Ride: assetInfo
  override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = assets.get(id)

  private lazy val resolveAlias: Map[Alias, Address] = for {
    (addr, state) <- input.accounts
    alias         <- state.aliases
  } yield alias -> addr

  // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
  override def resolveAlias(a: Alias): Either[ValidationError, Address] =
    resolveAlias.get(a).toRight(AliasDoesNotExist(a): ValidationError)

  // Ride: wavesBalance
  override def leaseBalance(address: Address): LeaseBalance = {
    val r = for {
      accountState <- input.accounts.get(address)
      lease        <- accountState.leasing
    } yield LeaseBalance(lease.in.value, lease.out.value)
    r.getOrElse(LeaseBalance(0, 0))
  }

  // Ride: assetBalance, wavesBalance
  override def balance(address: Address, mayBeAssetId: Asset): Long =
    input.accounts.get(address).flatMap(_.balance(mayBeAssetId)).getOrElse(0L)

  private val balanceSnapshotsCache = mkCache[Address, Seq[BalanceSnapshot]] { address =>
    val generatingBalance = input.accounts
      .get(address)
      .flatMap { addressState => addressState.generatingBalance.map(_.value).orElse(addressState.balance(Waves)) }
      .getOrElse(0L)

    Seq(BalanceSnapshot(height, generatingBalance, 0, 0))
  }

  // Ride: wavesBalance (specifies to=None)
  /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
  override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
    // "to" always None
    balanceSnapshotsCache.get(address).filter(_.height >= from)

  override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] =
    if (height < this.height) None
    else Some((this.height, balance(address, assetId)))

  private def complexityInfoOf(isAsset: Boolean, script: Script): ComplexityInfo =
    estimate(this, script, isAsset = isAsset, withCombinedContext = true)

  // Ride: transactionHeightById
  override def transactionMeta(id: ByteStr): Option[TxMeta] = input.transactions.get(id).map { tx =>
    TxMeta(
      height = Height(tx.height.getOrElse((height - 1).max(1))),
      TxMeta.Status.Succeeded,
      spentComplexity = 0
    )
  }

  override def carryFee(refId: Option[BlockId]): Long = ???

  override def transactionInfos(ids: Seq[BlockId]): Seq[Option[(TxMeta, Transaction)]] = ???

  override def leaseBalances(addresses: Seq[Address]): Map[Address, LeaseBalance] = ???

  override def balances(req: Seq[(Address, Asset)]): Map[(Address, Asset), Long] = ???

  override def wavesBalances(addresses: Seq[Address]): Map[Address, Long] = ???

  override def effectiveBalanceBanHeights(address: Address): Seq[Int] = ???

  override def lastStateHash(refId: Option[BlockId]): BlockId = ???

  // Ride: transferTransactionById
  override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
    input.transactions.get(id).map { inputTx =>
      val meta = transactionMeta(id).getOrElse(throw new RuntimeException(s"Can't find a metadata of the transaction $id"))
      val tx = TransferTransaction(
        version = inputTx.version,
        sender = inputTx.senderPublicKey,
        recipient = inputTx.recipient,
        assetId = inputTx.assetId,
        amount = TxPositiveAmount.from(inputTx.amount).explicitGet(),
        feeAssetId = inputTx.feeAssetId,
        fee = TxPositiveAmount.from(inputTx.fee).explicitGet(),
        attachment = ByteStr(inputTx.attachment),
        timestamp = inputTx.timestamp,
        proofs = Proofs(inputTx.proofs.map(ByteStr(_))),
        chainId = chainId
      )
      (meta.height, tx)
    }

  private def mkCache[K, V](f: K => V): LoadingCache[K, V] = Caffeine
    .newBuilder()
    .build[K, V](new CacheLoader[K, V] { override def load(key: K): V = f(key) })
}
