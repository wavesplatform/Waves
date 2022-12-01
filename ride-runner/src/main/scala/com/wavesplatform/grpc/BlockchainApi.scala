package com.wavesplatform.grpc

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.api.grpc.BalanceResponse.WavesBalances
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.grpc.BlockchainApi.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.state.{AssetDescription, DataEntry, Height}
import com.wavesplatform.transaction.Asset
import monix.reactive.Observable

trait BlockchainApi {
  def mkBlockchainUpdatesStream(): BlockchainUpdatesStream
  def getCurrentBlockchainHeight(): Int
  def getActivatedFeatures(height: Int): Map[Short, Int]
  def getAccountDataEntries(address: Address): Seq[DataEntry[?]]
  def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]]
  def getAccountScript(address: Address): Option[Script]
  def getBlockHeader(height: Int): Option[SignedBlockHeader]
  def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeader]
  def getVrf(height: Int): Option[ByteStr]
  def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription]
  def resolveAlias(alias: Alias): Option[Address]
  def getBalance(address: Address, asset: Asset): Long
  def getLeaseBalance(address: Address): WavesBalances
  def getTransactionHeight(id: ByteStr): Option[Height]
}

object BlockchainApi {
  trait BlockchainUpdatesStream extends AutoCloseable {
    val stream: Observable[WrappedEvent[SubscribeEvent]]

    def start(fromHeight: Int): Unit
    def start(fromHeight: Int, toHeight: Int): Unit

    def closeUpstream(): Unit
    def closeDownstream(): Unit
  }
}
