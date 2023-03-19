package com.wavesplatform.riderunner.storage

import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKeys.EmptyPublicKey
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.BlockchainApi
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.Script.ComplexityInfo
import com.wavesplatform.protobuf.transaction.PBTransactions.toVanillaScript
import com.wavesplatform.riderunner.storage.StorageContext.ReadWrite
import com.wavesplatform.riderunner.storage.persistent.PersistentCache
import com.wavesplatform.state.{AccountScriptInfo, Height}

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

  def append(atHeight: Height, account: PublicKey, newScript: ByteString)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val address       = account.toAddress(chainId)
    val vanillaScript = toVanillaScript(newScript)
    val scriptInfo    = vanillaScript.map(toAccountScriptInfo(account, _))
    append(atHeight, address, scriptInfo)
  }

  def undoAppend(toHeight: Height, account: PublicKey)(implicit ctx: ReadWrite): AffectedTags[TagT] =
    undoAppend(toHeight, account.toAddress(chainId))

  def rollback(toHeight: Height, account: PublicKey, newScript: ByteString)(implicit ctx: ReadWrite): AffectedTags[TagT] = {
    val address       = account.toAddress(chainId)
    val vanillaScript = toVanillaScript(newScript)
    val scriptInfo    = vanillaScript.map(toAccountScriptInfo(account, _))
    rollback(toHeight, address, scriptInfo)
  }

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
