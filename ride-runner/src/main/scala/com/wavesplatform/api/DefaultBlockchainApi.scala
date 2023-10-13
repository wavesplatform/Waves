package com.wavesplatform.api

import cats.syntax.option.*
import com.google.protobuf.empty.Empty
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.BlockchainApi.BlockchainUpdatesStream
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.grpc.BalanceResponse.Balance
import com.wavesplatform.api.grpc.{
  AccountRequest,
  AccountsApiGrpc,
  ActivationStatusRequest,
  AssetRequest,
  AssetsApiGrpc,
  BalanceResponse,
  BalancesRequest,
  BlockRangeRequest,
  BlockRequest,
  BlockWithHeight,
  BlockchainApiGrpc,
  BlocksApiGrpc,
  DataRequest,
  TransactionsApiGrpc,
  TransactionsByIdRequest
}
import com.wavesplatform.api.observers.{ManualGrpcObserver, MonixWrappedDownstream}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.blockchain.SignedBlockHeaderWithVrf
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.transaction.PBAmounts
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.protobuf.{AddressExt, ByteStrExt, ByteStringExt}
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, DataEntry, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.{ScorexLogging, StringBytes}
import io.grpc.*
import io.grpc.stub.ClientCalls
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.operators.extraSyntax.*
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{MulticastStrategy, Observable, OverflowStrategy}

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace

class DefaultBlockchainApi(
    settings: Settings,
    grpcApiChannel: Channel,
    blockchainUpdatesApiChannel: Channel
) extends BlockchainApi
    with ScorexLogging {
  override def mkBlockchainUpdatesStream(scheduler: Scheduler): BlockchainUpdatesStream = {
    val s = ConcurrentSubject[WrappedEvent[SubscribeEvent]](MulticastStrategy.publish)(scheduler)

    new BlockchainUpdatesStream {
      private val working         = new AtomicBoolean(true)
      private val currentUpstream = new AtomicReference(new ManualGrpcObserver[SubscribeRequest, SubscribeEvent])

      private val connectableDownstream = s
        .doOnNextAck { (_, ack) =>
          Task.fromFuture(ack).flatMap { _ =>
            Task {
              log.trace("Requesting next")
              currentUpstream.get().requestNext()
            }
          }
        }
        .asyncBoundary(OverflowStrategy.BackPressure(settings.blockchainUpdatesApi.bufferSize))
        // Guarantees that we won't receive any message after WrapperEvent.Failed until we subscribe again
        .timeoutOnSlowUpstream(settings.blockchainUpdatesApi.noDataTimeout)
        .doOnError {
          case _: UpstreamTimeoutException => Task(closeUpstream(description = "upstream timeout"))
          case e                           => Task(log.error("Unhandled", e))
        }
        // The only way this stream continues and propagates a Failed message
        .onErrorRestartWith { case e if working.get() => WrappedEvent.Failed(e) }
        .publish(scheduler)

      override val downstream: Observable[WrappedEvent[SubscribeEvent]] = connectableDownstream

      override def start(fromHeight: Height, toHeight: Height): Unit = {
        val observer = new MonixWrappedDownstream[SubscribeRequest, SubscribeEvent](s)
        currentUpstream.getAndSet(observer).close(ReplaceWithNewException)

        // How to know the source of data: you can wrap blockchainUpdatesApiChannel by MetadataUtils.newCaptureMetadataInterceptor
        // and get HTTP headers in Metadata.
        val call = blockchainUpdatesApiChannel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT)

        log.info("Start receiving updates from {}", fromHeight)
        // Works only once, see publish > unsafeMulticast > ConnectableObservable.unsafeMulticast
        connectableDownstream.connect()

        ClientCalls.asyncServerStreamingCall(call, SubscribeRequest(fromHeight = fromHeight, toHeight = toHeight), observer)
      }

      override def close(): Unit = if (working.compareAndSet(true, false)) {
        closeDownstream()
        closeUpstream(description = "close")
      }

      private def closeUpstream(description: String): Unit = currentUpstream.get().close(StopException(description))

      private def closeDownstream(): Unit = {
        log.info("Closing the downstream...")
        s.onComplete()
      }
    }
  }

  override def getCurrentBlockchainHeight(): Height = Height(
    ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_CURRENT_HEIGHT, CallOptions.DEFAULT),
      Empty()
    )
  ).tap { r => log.trace(s"getCurrentBlockchainHeight: $r") }

  override def getActivatedFeatures(height: Height): Map[Short, Height] =
    ClientCalls
      .blockingUnaryCall(
        grpcApiChannel.newCall(BlockchainApiGrpc.METHOD_GET_ACTIVATION_STATUS, CallOptions.DEFAULT),
        ActivationStatusRequest(height)
      )
      .features
      .flatMap { x =>
        if (x.blockchainStatus.isActivated) (x.id.toShort -> Height(x.activationHeight)).some
        else none
      }
      .toMap
      .tap(r => log.trace(s"getActivatedFeatures: found ${r.mkString(", ")}"))

  override def getAccountDataEntries(address: Address): Seq[DataEntry[?]] = ClientCalls
    .blockingServerStreamingCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
      DataRequest(address.toByteString)
    )
    .asScala
    .flatMap(_.entry)
    .map(toVanillaDataEntry)
    .toSeq
    .tap(r => log.trace(s"getAccountDataEntries($address): found ${r.length} elements"))

  override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
    firstOf(
      ClientCalls
        .blockingServerStreamingCall(
          grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
          DataRequest(address = address.toByteString, key = key)
        )
    )
      .map(x => toVanillaDataEntry(x.getEntry))
      .tap(r => log.trace(s"getAccountDataEntry($address, '$key'): ${r.toFoundStr("value", _.value)}"))

  override def getAccountScript(address: Address): Option[(PublicKey, Script)] = {
    val as = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_SCRIPT, CallOptions.DEFAULT),
      AccountRequest(address.toByteString)
    )

    toVanillaScript(as.scriptBytes)
      .map((as.publicKey.toPublicKey, _))
      .tap(r => log.trace(s"getAccountScript($address): ${r.toFoundStr("hash", _.hashCode())}"))
  }

  override def getBlockHeader(height: Height): Option[SignedBlockHeaderWithVrf] = {
    val x = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK, CallOptions.DEFAULT),
      BlockRequest(request = BlockRequest.Request.Height(height))
    )

    toVanilla(x).tap(r => log.trace(s"getBlockHeader($height): ${r.toFoundStr("id", _.header.id())}"))
  }

  override def getBlockHeaderRange(fromHeight: Height, toHeight: Height): List[SignedBlockHeaderWithVrf] = ClientCalls
    .blockingServerStreamingCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK_RANGE, CallOptions.DEFAULT),
      BlockRangeRequest(fromHeight = fromHeight, toHeight = toHeight)
    )
    .asScala
    .flatMap(toVanilla)
    .toList
    .tap(_ => log.trace(s"getBlockHeaderRange($fromHeight, $toHeight)"))

  override def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription] = {
    val xs =
      try
        ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(AssetsApiGrpc.METHOD_GET_INFO, CallOptions.DEFAULT),
            AssetRequest(asset.id.toByteString)
          )
          .some
      catch {
        case x: StatusException if x.getStatus == Status.NOT_FOUND => None
      }

    val r = xs.map { x =>
      AssetDescription(
        originTransactionId = asset.id,
        issuer = x.issuer.toPublicKey,
        name = x.name.toByteString,
        description = x.description.toByteString,
        decimals = x.decimals,
        reissuable = x.reissuable,
        totalVolume = x.totalVolume,
        script = for {
          pbScript <- x.script
          script   <- toVanillaScript(pbScript.scriptBytes)
        } yield AssetScriptInfo(script, pbScript.complexity),
        sponsorship = x.sponsorship,
        // All next fields are not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
        lastUpdatedAt = Height @@ 0,
        nft = false,
        sequenceInBlock = 0,
        issueHeight = Height @@ 0
      )
    }

    log.trace(s"getAssetDescription($asset): ${r.toFoundStr(_.toString)}")
    r
  }

  override def resolveAlias(alias: Alias): Option[Address] = {
    val xs =
      try
        ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(AccountsApiGrpc.METHOD_RESOLVE_ALIAS, CallOptions.DEFAULT),
            alias.name
          )
          .some
      catch {
        case e: StatusException if e.getStatus == Status.INVALID_ARGUMENT => None
      }

    val r = xs.map { x =>
      Address
        .fromBytes(x.toByteArray)
        .fold(
          e => throw new RuntimeException(s"Can't get address of alias '$alias': ${e.reason}"),
          identity
        )
    }

    log.trace(s"resolveAlias($alias): ${r.toFoundStr()}")
    r
  }

  override def getBalance(address: Address, asset: Asset): Long =
    getBalanceInternal(address, asset)
      .fold(0L) {
        case Balance.Empty               => 0L
        case Balance.Waves(wavesBalance) => wavesBalance.regular
        case Balance.Asset(assetBalance) => assetBalance.amount
      }
      .tap(r => log.trace(s"getBalance($address, $asset): $r"))

  override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances =
    getBalanceInternal(address, Asset.Waves)
      .fold(BalanceResponse.WavesBalances.defaultInstance) {
        case Balance.Waves(wavesBalance) => wavesBalance
        case x                           => throw new RuntimeException(s"Expected Balance.Waves, but got $x")
      }
      .tap(r => log.trace(s"getLeaseBalance($address): $r"))

  private def getBalanceInternal(address: Address, asset: Asset): Option[Balance] =
    firstOf(
      ClientCalls.blockingServerStreamingCall(
        grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_BALANCES, CallOptions.DEFAULT),
        BalancesRequest(address = address.toByteString, assets = Seq(PBAmounts.toPBAssetId(asset)))
      )
    ).map(_.balance)

  override def getTransactionHeight(id: ByteStr): Option[Height] = {
    val ths = firstOf(
      ClientCalls.blockingServerStreamingCall(
        grpcApiChannel.newCall(TransactionsApiGrpc.METHOD_GET_STATUSES, CallOptions.DEFAULT),
        TransactionsByIdRequest(transactionIds = Seq(id.toByteString))
      )
    )

    val r = ths.map(x => Height(x.height.toInt))
    log.trace(s"getTransactionHeight($id): ${r.toFoundStr { h => s"height=$h" }}")
    r
  }

  private def firstOf[T](xs: java.util.Iterator[T]): Option[T] = if (xs.hasNext) xs.next().some else none
}

object DefaultBlockchainApi {
  case class Settings(grpcApi: GrpcApiSettings, blockchainUpdatesApi: BlockchainUpdatesApiSettings)
  case class GrpcApiSettings(maxConcurrentRequests: Option[Int])
  case class BlockchainUpdatesApiSettings(noDataTimeout: FiniteDuration, bufferSize: Int)

  private case object ReplaceWithNewException           extends RuntimeException("Replace with a new observer") with NoStackTrace
  private case class StopException(description: String) extends RuntimeException(s"By a request: $description") with NoStackTrace

  def toVanilla(blockWithHeight: BlockWithHeight): Option[SignedBlockHeaderWithVrf] = for {
    b      <- blockWithHeight.block
    header <- b.header
    signature = b.signature
  } yield SignedBlockHeaderWithVrf(
    SignedBlockHeader(
      header = BlockHeader(
        version = header.version.toByte,
        timestamp = header.timestamp,
        reference = header.reference.toByteStr,
        baseTarget = header.baseTarget,
        generationSignature = header.generationSignature.toByteStr,
        generator = header.generator.toPublicKey,
        featureVotes = header.featureVotes.map(_.toShort),
        rewardVote = header.rewardVote,
        transactionsRoot = header.transactionsRoot.toByteStr
      ),
      signature = signature.toByteStr
    ),
    vrf = blockWithHeight.vrf.toByteStr,
    blockReward = blockWithHeight.rewardShares.map(_.reward).sum
  )
}
