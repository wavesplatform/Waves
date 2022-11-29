package com.wavesplatform.storage

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.grpc.BlockchainApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.ride.input.EmptyPublicKey
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.storage.AccountScriptStorage.toAccountScriptInfo
import com.wavesplatform.storage.actions.{AppendResult, RollbackResult}
import com.wavesplatform.storage.persistent.PersistentCache
import com.wavesplatform.utils.ScorexLogging

class AccountScriptStorage[TagT](
    chainId: Byte,
    estimator: => ScriptEstimator,
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[Address, AccountScriptInfo]
) extends ExactWithHeightStorage[Address, AccountScriptInfo, TagT] {
  override def getFromBlockchain(key: Address): Option[AccountScriptInfo] = {
    blockchainApi.getAccountScript(key).map { script =>
      toAccountScriptInfo(estimator, EmptyPublicKey, script) // EmptyPublicKey will be replaced during an update
    }
  }

  def append(height: Int, account: PublicKey, newScript: ByteString): AppendResult[TagT] =
    append(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(estimator, account, _)))

  def rollback(height: Int, account: PublicKey, newScript: ByteString): RollbackResult[TagT] =
    rollback(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(estimator, account, _)))
}

object AccountScriptStorage extends ScorexLogging {
  def toAccountScriptInfo(estimator: ScriptEstimator, account: PublicKey, script: Script): AccountScriptInfo = {
    // See DiffCommons
    // TODO #27 Get right arguments for Script.complexityInfo
    val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
    val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

    // TODO #4 explicitGet?
    val complexityInfo = Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()
    log.trace(s"Complexities (estimator of v${estimator.version}): ${complexityInfo.callableComplexities}")

    AccountScriptInfo(
      publicKey = account,
      script = script, // It looks like only this field matters in Ride Runner, see MutableBlockchain.accountScript
      verifierComplexity = complexityInfo.verifierComplexity,
      complexitiesByEstimator = Map(estimator.version -> complexityInfo.callableComplexities)
    )
  }
}
