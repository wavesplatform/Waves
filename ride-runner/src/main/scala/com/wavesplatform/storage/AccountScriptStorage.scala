package com.wavesplatform.storage

import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.riderunner.input.EmptyPublicKey
import com.wavesplatform.state.AccountScriptInfo
import com.wavesplatform.storage.actions.AffectedTags
import com.wavesplatform.storage.persistent.PersistentCache

class AccountScriptStorage[TagT](
    override val settings: ExactWithHeightStorage.Settings,
    chainId: Byte,
    estimate: Script => Map[Int, ComplexityInfo],
    blockchainApi: BlockchainApi,
    override val persistentCache: PersistentCache[Address, AccountScriptInfo]
) extends ExactWithHeightStorage[Address, AccountScriptInfo, TagT] {
  override def getFromBlockchain(key: Address): Option[AccountScriptInfo] = {
    blockchainApi.getAccountScript(key).map { script =>
      toAccountScriptInfo(EmptyPublicKey, script) // EmptyPublicKey will be replaced during an update
    }
  }

  def append(height: Int, account: PublicKey, newScript: ByteString): AffectedTags[TagT] =
    append(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(account, _)))

  def undoAppend(height: Int, account: PublicKey): AffectedTags[TagT] =
    undoAppend(height, account.toAddress(chainId))

  def rollback(height: Int, account: PublicKey, newScript: ByteString): AffectedTags[TagT] =
    rollback(height, account.toAddress(chainId), toVanillaScript(newScript).map(toAccountScriptInfo(account, _)))

  def toAccountScriptInfo(account: PublicKey, script: Script): AccountScriptInfo = {
    val estimated = estimate(script)
    AccountScriptInfo(
      publicKey = account,
      script = script, // See WavesEnvironment.accountScript
      verifierComplexity = estimated.maxBy { case (version, _) => version }._2.verifierComplexity,
      complexitiesByEstimator = estimated.map { case (version, x) => version -> x.callableComplexities } // "Cannot find complexity storage" if empty
    )
  }
}
