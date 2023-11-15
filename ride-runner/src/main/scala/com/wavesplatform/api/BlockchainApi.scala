package com.wavesplatform.api

import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi.*
import com.wavesplatform.api.grpc.BalanceResponse.WavesBalances
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{AssetDescription, DataEntry, Height}
import com.wavesplatform.transaction.Asset
import monix.execution.Scheduler
import monix.reactive.Observable

trait BlockchainApi {
  def mkBlockchainUpdatesStream(scheduler: Scheduler): BlockchainUpdatesStream
  def getCurrentBlockchainHeight(): Height
  def getActivatedFeatures(height: Height): Map[Short, Height]
  def getAccountDataEntries(address: Address): Seq[DataEntry[?]]
  def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]]
  def getAccountScript(address: Address): Option[(PublicKey, Script)]
  def getBlockHeader(height: Height): Option[SignedBlockHeaderWithVrf]
  def getBlockHeaderRange(fromHeight: Height, toHeight: Height): List[SignedBlockHeaderWithVrf]
  def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription]
  def resolveAlias(alias: Alias): Option[Address]
  def getBalance(address: Address, asset: Asset): Long
  def getLeaseBalance(address: Address): WavesBalances
  def getTransactionHeight(id: ByteStr): Option[Height]
}

object BlockchainApi {
  trait BlockchainUpdatesStream extends AutoCloseable {
    val downstream: Observable[WrappedEvent[SubscribeEvent]]

    def start(fromHeight: Height): Unit = start(fromHeight, toHeight = Height(0))
    def start(fromHeight: Height, toHeight: Height): Unit
  }
}
