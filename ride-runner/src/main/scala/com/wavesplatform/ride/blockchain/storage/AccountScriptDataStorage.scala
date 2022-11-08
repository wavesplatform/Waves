package com.wavesplatform.ride.blockchain.storage

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.grpc.BlockchainGrpcApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.ride.blockchain.DataKey.AccountScriptDataKey
import com.wavesplatform.ride.blockchain.caches.PersistentCache
import com.wavesplatform.ride.blockchain.storage.AccountScriptDataStorage.toAccountScriptInfo
import com.wavesplatform.ride.blockchain.{AppendResult, DataKey, RollbackResult}
import com.wavesplatform.state.AccountScriptInfo

class AccountScriptDataStorage[TagT](
    chainId: Byte,
    estimator: => ScriptEstimator,
    blockchainApi: BlockchainGrpcApi,
    override val persistentCache: PersistentCache[Address, AccountScriptInfo]
) extends DataStorage[Address, AccountScriptInfo, TagT] {
  override def mkDataKey(key: Address): DataKey = AccountScriptDataKey(key)

  override def getFromBlockchain(key: Address): Option[AccountScriptInfo] = blockchainApi.getAccountScript(key, estimator)

  def append(height: Int, account: PublicKey, newScript: ByteString): AppendResult[TagT] =
    append(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(estimator, account, _)))

  def rollback(height: Int, account: PublicKey, newScript: ByteString): RollbackResult[TagT] =
    // TODO copy-paste from append
    rollback(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(estimator, account, _)))
}

object AccountScriptDataStorage {
  def toAccountScriptInfo(estimator: ScriptEstimator, account: PublicKey, script: Script): AccountScriptInfo = {
    // TODO dup, see BlockchainGrpcApi

    // DiffCommons
    val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
    val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

    val complexityInfo = Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()

    AccountScriptInfo(
      publicKey = account,
      script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
      verifierComplexity = complexityInfo.verifierComplexity,
      complexitiesByEstimator = Map(estimator.version -> complexityInfo.callableComplexities)
    )
  }
}
