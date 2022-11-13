package com.wavesplatform.grpc

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.SignedBlockHeader
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.grpc.BlockchainGrpcApi.*
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, DataEntry, Portfolio, TxMeta}
import com.wavesplatform.transaction.{Asset, Transaction}
import monix.reactive.Observable

trait BlockchainGrpcApi {
  def mkBlockchainUpdatesStream(): BlockchainUpdatesStream
  def getCurrentBlockchainHeight(): Int
  def getActivatedFeatures(height: Int): Map[Short, Int]
  def getAccountDataEntries(address: Address): Seq[DataEntry[_]]
  def getAccountDataEntry(address: Address, key: String): Option[DataEntry[_]]
  def getAccountScript(address: Address, estimator: ScriptEstimator): Option[AccountScriptInfo]
  def getBlockHeader(height: Int): Option[SignedBlockHeader]
  def getVrf(height: Int): Option[ByteStr]
  def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription]
  def resolveAlias(alias: Alias): Option[Address]
  def getBalances(address: Address): Portfolio
  def getTransferLikeTransaction(id: ByteStr): Option[(TxMeta, Option[Transaction])]
}

object BlockchainGrpcApi {
  sealed trait Event extends Product with Serializable
  object Event {
    case class Next(event: SubscribeEvent) extends Event
    case class Failed(error: Throwable)    extends Event
    case object Closed                     extends Event
  }

  trait BlockchainUpdatesStream extends AutoCloseable {
    val stream: Observable[Event]

    def start(fromHeight: Int): Unit // TODO better name
    def start(fromHeight: Int, toHeight: Int): Unit

    def closeUpstream(): Unit
    def closeDownstream(): Unit
  }
}
