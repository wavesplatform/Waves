package com.wavesplatform.ride.runner

import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.http.utils.UtilsApiRoute
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.features.EstimatorProvider.EstimatorBlockchainExt
import com.wavesplatform.history.{DefaultBlockchainSettings, DefaultWavesSettings}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.ride.runner.blockchain.mkAccountScript
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{
  AccountScriptInfo,
  AssetDescription,
  AssetScriptInfo,
  BalanceSnapshot,
  Blockchain,
  DataEntry,
  LeaseBalance,
  TxMeta,
  VolumeAndFee
}
import com.wavesplatform.transaction.transfer.TransferTransactionLike
import com.wavesplatform.transaction.{Asset, ERC20Address, Transaction}
import com.wavesplatform.{BaseTestSuite, HasTestAccounts}
import play.api.libs.json.Json

import java.io.{PrintWriter, StringWriter}
import java.util.concurrent.atomic.AtomicInteger

class ScriptExecutionTestSuite extends BaseTestSuite with HasTestAccounts {
  private val DefaultHeight = 3565654

  "Blockchain.height is used only in script" - {
    "no height" in {
      val blockchain = new CountedHeightCalls
      UtilsApiRoute.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = aliceAddr,
        request = Json.obj("expr" -> "true"),
        trace = false,
        maxTxErrorLogSize = 0
      )
      blockchain.heightCalls.get() shouldBe 0
    }

    "one invoke" in {
      val blockchain = new CountedHeightCalls
      UtilsApiRoute.evaluate(
        evaluateScriptComplexityLimit = Int.MaxValue,
        blockchain = blockchain,
        address = aliceAddr,
        request = Json.obj("expr" -> s"height > $DefaultHeight"),
        trace = false,
        maxTxErrorLogSize = 0
      )
      blockchain.heightCalls.get() shouldBe 1
    }

    "invoke from invoke" in {}
  }

  private class CountedHeightCalls extends Blockchain {
    val heightCalls = new AtomicInteger(0)

    override def settings: BlockchainSettings = DefaultBlockchainSettings

    override def height: Int = {
      heightCalls.incrementAndGet()
      val sw = new StringWriter()
      new RuntimeException().printStackTrace(new PrintWriter(sw))
      val exceptionAsString = sw.toString
      println(s"Called height:\n$exceptionAsString")
      DefaultHeight
    }

    // TODO #16 We don't support it for now, use GET /utils/script/evaluate
    // Ride: isDataStorageUntouched
    override def hasData(address: Address): Boolean = kill(s"hasData($address)")

    // Ride: get*Value (data), get* (data)
    override def accountData(address: Address, key: String): Option[DataEntry[?]] = kill(s"accountData($address, $key)")

    // Ride: scriptHash
    override def accountScript(address: Address): Option[AccountScriptInfo] = Some(
      mkAccountScript(
        this.estimator,
        activatedFeatures,
        DefaultHeight,
        EmptyPublicKey,
        TestCompiler(V6).compileContract(
          """@Callable(inv)
            |func default() = {
            |  []
            |}""".stripMargin
        )
      )
    )

    // Indirectly
    override def hasAccountScript(address: Address): Boolean = kill(s"hasAccountScript($address)")

    // Ride: blockInfoByHeight, lastBlock
    override def blockHeader(height: Int): Option[SignedBlockHeader] = kill(s"blockHeader($height)")

    // Ride: blockInfoByHeight
    override def hitSource(height: Int): Option[ByteStr] = kill(s"hitSource($height)")

    override def activatedFeatures: Map[Short, Int] = DefaultWavesSettings.blockchainSettings.functionalitySettings.preActivatedFeatures

    // Ride: assetInfo
    override def assetDescription(id: Asset.IssuedAsset): Option[AssetDescription] = kill(s"assetDescription($id)")

    // Ride (indirectly): asset script validation
    override def assetScript(id: Asset.IssuedAsset): Option[AssetScriptInfo] = kill(s"assetScript($id)")

    // Ride: get*Value (data), get* (data), isDataStorageUntouched, balance, scriptHash, wavesBalance
    override def resolveAlias(a: Alias): Either[ValidationError, Address] = kill(s"resolveAlias($a)")

    // Ride: wavesBalance
    override def leaseBalance(address: Address): LeaseBalance = kill(s"leaseBalance($address)")

    // Ride: assetBalance, wavesBalance
    override def balance(address: Address, mayBeAssetId: Asset): Long = kill(s"balance($address, $mayBeAssetId)")

    // Retrieves Waves balance snapshot in the [from, to] range (inclusive)
    // Ride: wavesBalance (specifies to=None), "to" always None and means "to the end"
    override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] =
      kill(s"balanceSnapshots($address, $from, $to)")

    // Ride: transactionHeightById
    override def transactionMeta(id: ByteStr): Option[TxMeta] = kill(s"transactionMeta($id)")

    // Ride: transferTransactionById
    override def transferById(id: ByteStr): Option[(Int, TransferTransactionLike)] = kill(s"transferById($id)")

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

    // GET /eth/assets
    // TODO #99 see Keys.assetStaticInfo
    override def resolveERC20Address(address: ERC20Address): Option[Asset.IssuedAsset] = kill("resolveERC20Address")

    private def kill(methodName: String) = throw new RuntimeException(s"$methodName is not supported, contact with developers")
  }
}
