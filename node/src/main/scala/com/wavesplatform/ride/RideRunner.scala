package com.wavesplatform.ride

import com.google.protobuf.UnsafeByteOperations
import com.wavesplatform.Application
import com.wavesplatform.account.{Address, AddressScheme, Alias}
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.BlockchainFeatures.RideV6
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.{API, ValidationError}
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.diffs.invoke.InvokeScriptTransactionDiff
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
import com.wavesplatform.transaction.smart.script.trace.InvokeScriptTrace
import com.wavesplatform.transaction.transfer.{TransferTransaction, TransferTransactionLike}
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction, TxPositiveAmount}

import java.io.File
import scala.io.Source
import scala.util.Using

object RideRunner {
  /*
  seed: test

  Has a script
  Nonce is: 0
  Public key: Cq5itmx4wbYuogySAoUp58MimLLkQrFFLr1tpJy2BYp1
  Address in 'W': 3PCH3sUqeiPFAhrKzEnSEXoE2B6G9YNromV

  alice
  Nonce is: 1
  Public key: BWfushcMzh4YhHUjaHAW4iPUJHtCZ6SrpkDXtEhAiRQn
  Address in 'W': 3P6GhtTsABtYUgzhXTA4cDwbqqy7HqruiQQ

  bob
  Nonce is: 2
  Public key: 9K1Nu1udY4NAv77ktLqGAAxRtkL1epGA7tickpjDgPjP
  Address in 'W': 3PE7TH41wVuhn2SpAwWBBzeGxxzz8wXrb6L

  jane
  Nonce is: 3
  Public key: 5gmbkRC62E4YMX5RAnotUtpqccna8wPaNqCqo5hZsTeo
  Address in 'W': 3P4xDBqzXgR8HyXoyNn1C8Bd88h4rsEBMHA
   */
  def main(args: Array[String]): Unit = {
    val basePath     = args(0)
    val nodeSettings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    val input          = RideRunnerInput.parse(Using(Source.fromFile(new File(s"$basePath/input.json")))(_.getLines().mkString("\n")).get)
    val scriptSrc      = """
{-#STDLIB_VERSION 6 #-}
{-#SCRIPT_TYPE ACCOUNT #-}
{-#CONTENT_TYPE DAPP #-}

@Callable(inv)
func foo(x: Int) = {
  let alice = Address(base58'3P6GhtTsABtYUgzhXTA4cDwbqqy7HqruiQQ')
  let carl = addressFromRecipient(Alias("carl"))
  let bob = Address(base58'3PE7TH41wVuhn2SpAwWBBzeGxxzz8wXrb6L')
  let jane = Address(base58'3P4xDBqzXgR8HyXoyNn1C8Bd88h4rsEBMHA')

  let asset = base58'8LQW8f7P5d5PZM7GtZEBgaqRPGSzS3DfPuiXrURJ4AJS'
  let txId = base58'8rc5Asw43qbq7LMZ6tu2aVbVkw72XmBt7tTnwMSNfaNq'

  # Functions
  let x1 = getIntegerValue(alice, "a")
  let x2 = if (isDataStorageUntouched(carl)) then 1 else 0
  let x3 = assetBalance(bob, asset)
  let x4 = value(assetInfo(asset)).decimals
  let x5 = value(blockInfoByHeight(3296627)).height
  let x6 = size(value(scriptHash(this)))
  let x7 = value(transactionHeightById(txId))
  let x8 = value(transferTransactionById(txId)).amount
  let x9 = wavesBalance(carl).available
  let x10 = invoke(this, "bar", [], []).exactAs[Int]

  # Vals
  let y1 = height
  let y2 = lastBlock.height

  ([ScriptTransfer(bob, 1, asset)], x + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + y1 + y2)
}

@Callable(inv)
func bar() = {
  let x1 = if (valueOrElse(getBoolean("b"), false)) then 1 else 0
  ([], x1)
}"""
    val estimator      = ScriptEstimatorV3(fixOverflow = true, overhead = false)
    val compiledScript = API.compile(input = scriptSrc, estimator).explicitGet()

    def kill(methodName: String) = throw new RuntimeException(methodName)
    // TODO default values?
    val blockchain: Blockchain = new Blockchain {
      // Ride: isDataStorageUntouched
      override def hasData(address: Address): Boolean = input.hasData.getOrElse(address, throw new RuntimeException(s"hasData($address)"))

      // Ride: scriptHash
      override def accountScript(address: Address): Option[AccountScriptInfo] = {
        input.accountScript.get(address).map { input =>
          val complexityInfo = Seq(ScriptEstimatorV1, ScriptEstimatorV2, this.estimator).map { estimator =>
            estimator.version -> complexityInfoOf(address.toString, input.script)
          }

          val (lastEstimatorVersion, lastComplexityInfo) = complexityInfo.last
          val r = AccountScriptInfo(
            script = input.script,
            publicKey = input.publicKey,
            verifierComplexity = lastComplexityInfo.verifierComplexity,
            complexitiesByEstimator = complexityInfo
              .map { case (v, complexityInfo) => v -> complexityInfo.callableComplexities }
              .toMap
              .updated(lastEstimatorVersion, lastComplexityInfo.callableComplexities) // to preserve
          )

          if (address.toString == "3PCH3sUqeiPFAhrKzEnSEXoE2B6G9YNromV") {
            r.copy(
              script = Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet(),
              verifierComplexity = compiledScript.verifierComplexity,
              complexitiesByEstimator = Map(
                estimator.version -> compiledScript.callableComplexities
              )
            )
          } else r
        }
      }

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

      // Ride: wavesBalance (specifies to=None)
      /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
      override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
        // "to" always None
        input.balanceSnapshots.getOrElse(address, Seq(BalanceSnapshot(height, 0, 0, 0))).filter(_.height >= from)

      override def settings: BlockchainSettings = nodeSettings.blockchainSettings

      // Ride: wavesBalance, height, lastBlock TODO: a binding in Ride?
      override def height: Int = input.height

      override def activatedFeatures: Map[Short, Int] = input.activatedFeatures

      private lazy val assets: Map[IssuedAsset, AssetDescription] = input.assets.map { case (asset, info) =>
        asset -> AssetDescription(
          originTransactionId = asset.id,
          issuer = info.issuerPublicKey,
          name = UnsafeByteOperations.unsafeWrap(info.name.arr),               // TODO allow to specify base58
          description = UnsafeByteOperations.unsafeWrap(info.description.arr), // TODO allow to specify base58
          decimals = info.decimals,
          reissuable = info.reissuable,
          totalVolume = info.quantity,
          lastUpdatedAt = Height(1),
          script = info.script.map { script =>
            val complexityInfo = complexityInfoOf(asset.toString, script)
            AssetScriptInfo(script, complexityInfo.verifierComplexity)
          },
          sponsorship = info.minSponsoredAssetFee,
          nft = this.isFeatureActivated(BlockchainFeatures.ReduceNFTFee) && info.quantity == 1 && info.decimals == 0 && !info.reissuable
        )
      }

      private def complexityInfoOf(label: String, script: Script): ComplexityInfo =
        Script.complexityInfo(
          script,
          this.estimator,
          fixEstimateOfVerifier = this.isFeatureActivated(RideV6),
          useContractVerifierLimit = false,
          withCombinedContext = true
        ) match {
          case Right(x) => x
          case Left(e)  => throw new RuntimeException(s"Can't get a complexity info of '$label' script: $e")
        }

      // Ride: assetInfo
      override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = assets.get(id)

      // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
      override def resolveAlias(a: Alias): Either[ValidationError, Address] =
        input.resolveAlias.get(a).toRight(AliasDoesNotExist(a): ValidationError)

      // Ride: get*Value (data), get* (data)
      /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
      override def accountData(acc: Address, key: String): Option[DataEntry[_]] =
        input.accountData.getOrElse(acc, Map.empty).get(key)

      // Ride: wavesBalance
      override def leaseBalance(address: Address): LeaseBalance = input.leaseBalance.getOrElse(address, LeaseBalance(0, 0))

      // Ride: assetBalance, wavesBalance
      override def balance(address: Address, mayBeAssetId: Asset): Long =
        input.balance.get(address).flatMap(_.get(mayBeAssetId)).getOrElse(0)

      // Ride: transactionHeightById
      override def transactionMeta(id: ByteStr): Option[TxMeta] = input.transactionMeta.get(id)

      lazy val transferById: Map[ByteStr, TransferTransactionLike] = for {
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
        proofs = tx.proofs,
        chainId = settings.addressSchemeCharacter.toByte
      )

      // Ride: transferTransactionById
      override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] =
        transferById.get(id).map { tx =>
          val meta = transactionMeta(id).getOrElse(throw new RuntimeException(s"Can't find a metadata of the transaction $id"))
          (meta.height, tx)
        }

      // Ride (indirectly): asset script validation
      override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = assets.get(id).flatMap(_.script)

      override def hasAccountScript(address: Address) = kill("hasAccountScript")

      override def score: BigInt = kill("score")

      override def carryFee: Long = kill("carryFee")

      override def heightOf(blockId: ByteStr): Option[Int] = kill("heightOf")

      /** Features related */
      override def approvedFeatures: Map[Short, Int] = kill("approvedFeatures")

      override def featureVotes(height: Int): Map[Short, Int] = kill("featureVotes")

      override def containsTransaction(tx: Transaction): Boolean = kill("containsTransaction")

      override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = kill("leaseDetails")

      override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = kill("filledVolumeAndFee")

      override def transactionInfo(id: BlockId) = kill("transactionInfo")

      /** Block reward related */
      override def blockReward(height: Int): Option[Long] = kill("blockReward")

      override def blockRewardVotes(height: Int): Seq[Long] = kill("blockRewardVotes")

      override def wavesAmount(height: Int): BigInt = kill("wavesAmount")

      override def balanceAtHeight(address: Address, height: Int, assetId: Asset): Option[(Int, Long)] = kill("balanceAtHeight")

      override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")
    }

    val apiResult = execute(blockchain, input.request)
    println(s"apiResult: $apiResult")
  }

  def execute(
      blockchain: Blockchain,
      request: RunnerRequest
  ): Either[ValidationError, Any] = {
    val result = InvokeScriptTransactionDiff(
      blockchain,
      System.currentTimeMillis(), // blockTime
      limitedExecution = false
    )(request.toTx(blockchain.settings.addressSchemeCharacter.toByte))
    // TODO trace
    result.resultE.map { all =>
      result.trace
        .collect { case x: InvokeScriptTrace => x.resultE.map(x => (x.returnedValue, all)) }
        .mkString("\n")
    }
  }
}
