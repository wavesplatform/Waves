package com.wavesplatform.api

import com.wavesplatform.account.{Address, Alias}
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
  def getCurrentBlockchainHeight(): Int
  def getActivatedFeatures(height: Int): Map[Short, Int]
  def getAccountDataEntries(address: Address): Seq[DataEntry[?]]
  def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]]
  def getAccountScript(address: Address): Option[Script]
  def getBlockHeader(height: Int): Option[SignedBlockHeaderWithVrf]
  def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeaderWithVrf]
  def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription]
  def resolveAlias(alias: Alias): Option[Address]
  def getBalance(address: Address, asset: Asset): Long
  def getLeaseBalance(address: Address): WavesBalances
  def getTransactionHeight(id: ByteStr): Option[Height]
}

object BlockchainApi {
  trait BlockchainUpdatesStream extends AutoCloseable {
    val downstream: Observable[WrappedEvent[SubscribeEvent]]

    def start(fromHeight: Int): Unit = start(fromHeight, toHeight = 0)
    def start(fromHeight: Int, toHeight: Int): Unit

    def closeUpstream(): Unit
    def closeDownstream(): Unit
  }
}
