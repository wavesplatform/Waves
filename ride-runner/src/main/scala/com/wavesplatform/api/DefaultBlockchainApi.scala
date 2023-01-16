package com.wavesplatform.api

import cats.syntax.option.*
import com.google.protobuf.empty.Empty
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias, PublicKey}
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
  BlockchainApiGrpc,
  BlocksApiGrpc,
  DataRequest,
  TransactionsApiGrpc,
  TransactionsByIdRequest
}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.collections.syntax.*
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.concurrent.MayBeSemaphore
import com.wavesplatform.events.WrappedEvent
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.api.BlockchainApi.BlockchainUpdatesStream
import com.wavesplatform.api.DefaultBlockchainApi.*
import com.wavesplatform.api.observers.{ManualGrpcObserver, MonixWrappedDownstream}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.state.{AssetDescription, AssetScriptInfo, DataEntry, Height}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.*
import io.grpc.stub.ClientCalls
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.exceptions.UpstreamTimeoutException
import monix.reactive.operators.extraSyntax.*
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{MulticastStrategy, Observable, OverflowStrategy}
import play.api.libs.json.{Json, Reads}
import sttp.client3.{Identity, SttpBackend, UriContext, asString, basicRequest}

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.NoStackTrace

class DefaultBlockchainApi(
    settings: Settings,
    grpcApiChannel: Channel,
    blockchainUpdatesApiChannel: Channel,
    httpBackend: SttpBackend[Identity, Any]
) extends ScorexLogging
    with BlockchainApi {

  private val grpcApiCalls = MayBeSemaphore(settings.grpcApi.maxConcurrentRequests)

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
          case _: UpstreamTimeoutException => Task(closeUpstream())
          case _                           => Task.unit
        }
        .onErrorRestartWith { case e: UpstreamTimeoutException if working.get() => WrappedEvent.Failed(e) }
        .publish(scheduler)

      override val downstream: Observable[WrappedEvent[SubscribeEvent]] = connectableDownstream

      override def start(fromHeight: Int, toHeight: Int): Unit = {
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

      override def closeUpstream(): Unit = currentUpstream.get().close(StopException)

      override def closeDownstream(): Unit = {
        log.info("Closing the downstream...")
        s.onComplete()
      }

      override def close(): Unit = if (working.compareAndSet(true, false)) {
        closeDownstream()
        closeUpstream()
      }
    }
  }

  override def getCurrentBlockchainHeight(): Int =
    grpcApiCalls
      .limited {
        ClientCalls.blockingUnaryCall(
          grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_CURRENT_HEIGHT, CallOptions.DEFAULT),
          Empty()
        )
      }
      .tap { r => log.trace(s"getCurrentBlockchainHeight: $r") }

  override def getActivatedFeatures(height: Int): Map[Short, Int] =
    grpcApiCalls
      .limited {
        ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(BlockchainApiGrpc.METHOD_GET_ACTIVATION_STATUS, CallOptions.DEFAULT),
            ActivationStatusRequest(height)
          )
      }
      .features
      .map(x => x.id.toShort -> x.activationHeight) // TODO #3 is this correct?
      .toMap
      .tap(r => log.trace(s"getActivatedFeatures: found ${r.mkString(", ")}"))

  override def getAccountDataEntries(address: Address): Seq[DataEntry[?]] =
    grpcApiCalls
      .limited {
        ClientCalls
          .blockingServerStreamingCall(
            grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
            DataRequest(toPb(address))
          )
      }
      .asScala
      .flatMap(_.entry)
      .map(toVanillaDataEntry)
      .toSeq
      .tap(r => log.trace(s"getAccountDataEntries($address): found ${r.length} elements"))

  override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[?]] =
    grpcApiCalls
      .limited {
        firstOf(
          ClientCalls
            .blockingServerStreamingCall(
              grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
              DataRequest(address = toPb(address), key = key)
            )
        )
      }
      .map(x => toVanillaDataEntry(x.getEntry))
      .tap(r => log.trace(s"getAccountDataEntry($address, '$key'): ${r.toFoundStr("value", _.value)}"))

  override def getAccountScript(address: Address): Option[Script] = {
    val x = grpcApiCalls.limited {
      ClientCalls.blockingUnaryCall(
        grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_SCRIPT, CallOptions.DEFAULT),
        AccountRequest(toPb(address))
      )
    }

    toVanillaScript(x.scriptBytes).tap(r => log.trace(s"getAccountScript($address): ${r.toFoundStr("hash", _.hashCode())}"))
  }

  override def getBlockHeader(height: Int): Option[SignedBlockHeader] = {
    val x = grpcApiCalls.limited {
      ClientCalls.blockingUnaryCall(
        grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK, CallOptions.DEFAULT),
        BlockRequest(request = BlockRequest.Request.Height(height))
      )
    }

    x.block.map(toVanilla).tap(r => log.trace(s"getBlockHeader($height): ${r.toFoundStr("id", _.id())}"))
  }

  override def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeader] = {
    val xs = grpcApiCalls.limited {
      ClientCalls.blockingServerStreamingCall(
        grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK_RANGE, CallOptions.DEFAULT),
        BlockRangeRequest(fromHeight = fromHeight, toHeight = toHeight)
      )
    }

    xs.asScala
      .map(x => toVanilla(x.getBlock))
      .toList
      .tap(_ => log.trace(s"getBlockHeaderRange($fromHeight, $toHeight)"))
  }

  // TODO #5: It seems VRF only from REST API, take from gRPC/BlockchainUpdates API
  override def getVrf(height: Int): Option[ByteStr] = {
    basicRequest
      .get(uri"${settings.nodeApiBaseUri}/blocks/headers/at/$height")
      .response(asString)
      .send(httpBackend)
      .body match {
      case Left(e) =>
        log.warn(s"Can't get VRF for $height: ${e.take(100)}")
        None

      case Right(rawJson) =>
        Json.parse(rawJson).as[HttpBlockHeader].VRF
    }
  }

  override def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription] = {
    val x = grpcApiCalls.limited {
      try
        ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(AssetsApiGrpc.METHOD_GET_INFO, CallOptions.DEFAULT),
            AssetRequest(UnsafeByteOperations.unsafeWrap(asset.id.arr))
          )
          .some
      catch {
        case x: StatusException if x.getStatus == Status.NOT_FOUND => None
      }
    }

    val r = x.map { x =>
      AssetDescription(
        originTransactionId = asset.id,
        issuer = PublicKey(x.issuer.toByteArray),
        name = UnsafeByteOperations.unsafeWrap(x.name.getBytes(StandardCharsets.UTF_8)),
        description = UnsafeByteOperations.unsafeWrap(x.description.getBytes(StandardCharsets.UTF_8)),
        decimals = x.decimals,
        reissuable = x.reissuable,
        totalVolume = x.totalVolume,
        lastUpdatedAt = Height(1), // not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
        script = for {
          pbScript <- x.script
          script   <- toVanillaScript(pbScript.scriptBytes)
        } yield AssetScriptInfo(script, pbScript.complexity),
        sponsorship = x.sponsorship,
        nft = false // not used, see: https://docs.waves.tech/en/ride/structures/common-structures/asset#fields
      )
    }

    log.trace(s"getAssetDescription($asset): ${r.toFoundStr(_.toString)}")
    r
  }

  override def resolveAlias(alias: Alias): Option[Address] = {
    val x = grpcApiCalls.limited {
      try
        ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(AccountsApiGrpc.METHOD_RESOLVE_ALIAS, CallOptions.DEFAULT),
            alias.toString // TODO #6: check implementation
          )
          .some
      catch {
        case e: StatusException if e.getStatus == Status.INVALID_ARGUMENT => None
      }
    }

    val r = x.map { x =>
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

  override def getLeaseBalance(address: Address): BalanceResponse.WavesBalances = {
    getBalanceInternal(address, Asset.Waves)
      .fold(BalanceResponse.WavesBalances.defaultInstance) {
        case Balance.Waves(wavesBalance) => wavesBalance
        case x                           => throw new RuntimeException(s"Expected Balance.Waves, but got $x")
      }
      .tap(r => log.trace(s"getLeaseBalance($address): $r"))
  }

  private def getBalanceInternal(address: Address, asset: Asset): Option[Balance] =
    grpcApiCalls
      .limited {
        firstOf(
          ClientCalls.blockingServerStreamingCall(
            grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_BALANCES, CallOptions.DEFAULT),
            BalancesRequest(address = toPb(address), assets = Seq(toPb(asset)))
          )
        )
      }
      .map(_.balance)

  override def getTransactionHeight(id: ByteStr): Option[Height] = {
    val x = grpcApiCalls.limited {
      firstOf(
        ClientCalls
          .blockingServerStreamingCall(
            grpcApiChannel.newCall(TransactionsApiGrpc.METHOD_GET_STATUSES, CallOptions.DEFAULT),
            TransactionsByIdRequest(transactionIds = Seq(UnsafeByteOperations.unsafeWrap(id.arr)))
          )
      )
    }

    val r = x.map(x => Height(x.height.toInt))
    log.trace(s"getTransactionHeight($id): ${r.toFoundStr { h => s"height=$h" }}")
    r
  }

  private def firstOf[T](xs: java.util.Iterator[T]): Option[T] = if (xs.hasNext) xs.next().some else none
}

object DefaultBlockchainApi {
  case class Settings(nodeApiBaseUri: String, grpcApi: GrpcApiSettings, blockchainUpdatesApi: BlockchainUpdatesApiSettings)
  case class GrpcApiSettings(maxConcurrentRequests: Option[Int])
  case class BlockchainUpdatesApiSettings(noDataTimeout: FiniteDuration, bufferSize: Int)

  private case class HttpBlockHeader(VRF: Option[ByteStr] = None)

  private object HttpBlockHeader {
    import com.wavesplatform.utils.byteStrFormat
    implicit val httpBlockHeaderReads: Reads[HttpBlockHeader] = Json.using[Json.WithDefaultValues].reads
  }

  private case object StopException           extends RuntimeException("By a request") with NoStackTrace
  private case object ReplaceWithNewException extends RuntimeException("Replace with a new observer") with NoStackTrace

  def toPb(address: Address): ByteString = UnsafeByteOperations.unsafeWrap(address.bytes)
  def toPb(asset: Asset): ByteString     = asset.fold(ByteString.EMPTY)(x => UnsafeByteOperations.unsafeWrap(x.id.arr))

  def toVanilla(block: Block): SignedBlockHeader = {
    val header = block.getHeader
    SignedBlockHeader(
      header = BlockHeader(
        version = header.version.toByte,
        timestamp = header.timestamp,
        reference = header.reference.toByteStr,
        baseTarget = header.baseTarget,
        generationSignature = header.generationSignature.toByteStr,
        generator = PublicKey(header.generator.toByteArray),
        featureVotes = header.featureVotes.map(_.toShort),
        rewardVote = header.rewardVote,
        transactionsRoot = header.transactionsRoot.toByteStr
      ),
      signature = block.signature.toByteStr
    )
  }
}
