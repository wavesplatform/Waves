package com.wavesplatform.grpc

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
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.grpc.BlockchainApi.{BlockchainUpdatesStream, Event}
import com.wavesplatform.grpc.DefaultBlockchainApi.{HttpBlockHeader, Settings}
import com.wavesplatform.grpc.observers.RichGrpcObserver
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.block.Block
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.ride.input.EmptyPublicKey
import com.wavesplatform.state.{AccountScriptInfo, AssetDescription, AssetScriptInfo, DataEntry, Height, LeaseBalance, Portfolio}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.utils.ScorexLogging
import io.grpc.*
import io.grpc.stub.ClientCalls
import monix.execution.Scheduler
import monix.reactive.MulticastStrategy
import monix.reactive.subjects.ConcurrentSubject
import play.api.libs.json.{Json, Reads}
import sttp.client3.{Identity, SttpBackend, UriContext, asString, basicRequest}

import java.nio.charset.StandardCharsets
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.chaining.scalaUtilChainingOps

class DefaultBlockchainApi(
    settings: Settings,
    grpcApiChannel: ManagedChannel,
    blockchainUpdatesApiChannel: ManagedChannel,
    httpBackend: SttpBackend[Identity, Any],
    hangScheduler: ScheduledExecutorService
) extends ScorexLogging
    with BlockchainApi {

  override def mkBlockchainUpdatesStream(): BlockchainUpdatesStream = {
    val s = ConcurrentSubject[Event](MulticastStrategy.publish)(Scheduler(hangScheduler))

    def mkObserver(call: ClientCall[SubscribeRequest, SubscribeEvent]): RichGrpcObserver[SubscribeRequest, SubscribeEvent] =
      new RichGrpcObserver[SubscribeRequest, SubscribeEvent](settings.noDataTimeout, hangScheduler) {
        override def onReceived(event: SubscribeEvent): Boolean = {
          s.onNext(Event.Next(event))
          true // TODO #2 make a fair back-pressure
        }

        override def onFailed(error: Throwable): Unit = {
          s.onNext(Event.Failed(error))
        }

        // We can't obtain IP earlier, it's a netty-grpc limitation
        override def onReady(): Unit = {
          // This doesn't work for streams :(
          // log.debug("Ready to receive from {}", call.socketAddressStr)
        }

        override def onClosedByRemotePart(): Unit = {
          s.onNext(Event.Closed)
        }
      }

    new BlockchainUpdatesStream {
      private val currentObserver = new AtomicReference[RichGrpcObserver[SubscribeRequest, SubscribeEvent]]
      override val stream         = s

      override def start(fromHeight: Int): Unit = start(fromHeight, toHeight = 0)

      override def start(fromHeight: Int, toHeight: Int): Unit = {
        val call     = blockchainUpdatesApiChannel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT)
        val observer = mkObserver(call)
        Option(currentObserver.getAndSet(observer)).foreach(_.close())

        log.info("Start receiving updates from {}", fromHeight)
        ClientCalls.asyncServerStreamingCall(call, SubscribeRequest(fromHeight = fromHeight, toHeight = toHeight), observer.underlying)
      }

      override def closeUpstream(): Unit = Option(currentObserver.get()).foreach(_.close()) // Closes if failed

      override def closeDownstream(): Unit = stream.onComplete()

      override def close(): Unit = {
        closeDownstream()
        closeUpstream()
      }
    }
  }

  override def getCurrentBlockchainHeight(): Int =
    ClientCalls
      .blockingUnaryCall(
        grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_CURRENT_HEIGHT, CallOptions.DEFAULT),
        Empty()
      )
      .tap(r => log.trace(s"getCurrentBlockchainHeight: $r"))

  override def getActivatedFeatures(height: Int): Map[Short, Int] =
    ClientCalls
      .blockingUnaryCall(
        grpcApiChannel.newCall(BlockchainApiGrpc.METHOD_GET_ACTIVATION_STATUS, CallOptions.DEFAULT),
        ActivationStatusRequest(height)
      )
      .features
      .map(x => x.id.toShort -> x.activationHeight) // TODO #3 is this correct?
      .toMap
      .tap(r => log.trace(s"getActivatedFeatures: found ${r.mkString(", ")}"))

  override def getAccountDataEntries(address: Address): Seq[DataEntry[_]] =
    try
      ClientCalls
        .blockingServerStreamingCall(
          grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
          DataRequest(toPb(address))
        )
        .asScala
        .flatMap(_.entry)
        .map(toVanillaDataEntry)
        .toSeq
        .tap(r => log.trace(s"getAccountDataEntries($address): found ${r.length} elements"))
    catch {
      case e: Throwable =>
        log.error(s"getAccountDataEntries($address)", e)
        throw e
    }

  override def getAccountDataEntry(address: Address, key: String): Option[DataEntry[_]] =
    try {
      ClientCalls
        .blockingServerStreamingCall(
          grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
          DataRequest(address = toPb(address), key = key)
        )
        .asScala
        .flatMap(_.entry)
        .map(toVanillaDataEntry)
        .take(1)
        .toSeq
        .headOption
        .tap(r => log.trace(s"getAccountDataEntry($address, '$key'): ${r.toFoundStr("value", _.value)}"))
    } catch {
      case e: Throwable =>
        log.error(s"getAccountDataEntry($address, '$key')", e)
        throw e
    }

  override def getAccountScript(address: Address, estimator: ScriptEstimator): Option[AccountScriptInfo] = {
    val x = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_SCRIPT, CallOptions.DEFAULT),
      AccountRequest(toPb(address))
    )

    toVanillaScript(x.scriptBytes)
      .map { script =>
        // DiffCommons
        val fixEstimateOfVerifier    = true // blockchain.isFeatureActivated(BlockchainFeatures.RideV6)
        val useContractVerifierLimit = true // !isAsset && blockchain.useReducedVerifierComplexityLimit

        // TODO #4 explicitGet?
        val complexityInfo = Script.complexityInfo(script, estimator, fixEstimateOfVerifier, useContractVerifierLimit).explicitGet()
        log.trace(s"Complexities (estimator of v${estimator.version}): ${complexityInfo.callableComplexities}")

        AccountScriptInfo(
          publicKey = EmptyPublicKey,
          script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
          verifierComplexity = x.complexity,
          complexitiesByEstimator = Map(estimator.version -> complexityInfo.callableComplexities)
        )
      }
      .tap(r => log.trace(s"getAccountScript($address): ${r.toFoundStr("hash", _.script.hashCode())}"))
  }

  override def getBlockHeader(height: Int): Option[SignedBlockHeader] = {
    val x = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK, CallOptions.DEFAULT),
      BlockRequest(request = BlockRequest.Request.Height(height))
    )

    x.block.map(toVanilla).tap(r => log.trace(s"getBlockHeader($height): ${r.toFoundStr("id", _.id())}"))
  }

  override def getBlockHeaderRange(fromHeight: Int, toHeight: Int): List[SignedBlockHeader] = {
    val xs = ClientCalls.blockingServerStreamingCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK_RANGE, CallOptions.DEFAULT),
      BlockRangeRequest(fromHeight = fromHeight, toHeight = toHeight)
    )

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
    val r =
      try {
        val x = ClientCalls
          .blockingUnaryCall(
            grpcApiChannel.newCall(AssetsApiGrpc.METHOD_GET_INFO, CallOptions.DEFAULT),
            AssetRequest(UnsafeByteOperations.unsafeWrap(asset.id.arr))
          )

        Some(
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
        )
      } catch {
        case x: StatusException if x.getStatus == Status.NOT_FOUND => None
      }

    log.trace(s"getAssetDescription($asset): ${r.toFoundStr(_.toString)}")
    r
  }

  override def resolveAlias(alias: Alias): Option[Address] = {
    val r =
      try {
        val x = ClientCalls.blockingUnaryCall(
          grpcApiChannel.newCall(AccountsApiGrpc.METHOD_RESOLVE_ALIAS, CallOptions.DEFAULT),
          alias.toString // TODO #6: check implementation
        )

        Address.fromBytes(x.toByteArray).toOption
      } catch {
        case e: StatusException if e.getStatus == Status.INVALID_ARGUMENT => None
      }

    log.trace(s"resolveAlias($alias): ${r.toFoundStr()}")
    r
  }

  override def getBalances(address: Address): Portfolio = {
    val xs = ClientCalls.blockingServerStreamingCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_BALANCES, CallOptions.DEFAULT),
      BalancesRequest(toPb(address))
    )

    xs.asScala
      .foldLeft(Portfolio.empty) { case (r, x) =>
        x.balance match {
          case Balance.Empty => r
          case Balance.Waves(wavesBalance) =>
            r.copy(
              balance = wavesBalance.regular,
              lease = LeaseBalance(in = wavesBalance.leaseIn, out = wavesBalance.leaseOut)
            )
          case Balance.Asset(assetBalance) =>
            r.copy(
              assets = r.assets.updated(Asset.IssuedAsset(assetBalance.assetId.toByteStr), assetBalance.amount)
            )
        }
      }
      .tap(r => log.trace(s"getBalances($address): found assets=${r.assets.size}"))
  }

  override def getTransactionHeight(id: ByteStr): Option[Height] = {
    val xs = ClientCalls
      .blockingServerStreamingCall(
        grpcApiChannel.newCall(TransactionsApiGrpc.METHOD_GET_STATUSES, CallOptions.DEFAULT),
        TransactionsByIdRequest(transactionIds = Seq(UnsafeByteOperations.unsafeWrap(id.arr)))
      )

    val r = if (xs.hasNext) {
      val pbTx = xs.next()
      Height(pbTx.height.toInt).some
    } else None

    log.trace(s"getTransactionHeight($id): ${r.toFoundStr { h => s"height=$h" }}")
    r
  }

  private def toPb(address: Address): ByteString = UnsafeByteOperations.unsafeWrap(address.bytes)

  private def toVanilla(block: Block): SignedBlockHeader = {
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

object DefaultBlockchainApi {
  case class Settings(nodeApiBaseUri: String, noDataTimeout: FiniteDuration)

  private case class HttpBlockHeader(VRF: Option[ByteStr] = None)

  private object HttpBlockHeader {

    import com.wavesplatform.utils.byteStrFormat

    implicit val httpBlockHeaderReads: Reads[HttpBlockHeader] = Json.using[Json.WithDefaultValues].reads
  }
}
