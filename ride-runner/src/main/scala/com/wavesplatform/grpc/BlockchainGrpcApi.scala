package com.wavesplatform.grpc

import cats.syntax.option.*
import com.google.protobuf.empty.Empty
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, Alias, PublicKey}
import com.wavesplatform.api.grpc.BalanceResponse.Balance
import com.wavesplatform.api.grpc.{
  AccountRequest,
  AccountsApiGrpc,
  AssetRequest,
  AssetsApiGrpc,
  BalancesRequest,
  BlockRequest,
  BlocksApiGrpc,
  DataRequest
}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.grpc.BlockchainGrpcApi.{Event, Settings}
import com.wavesplatform.grpc.observers.RichGrpcObserver
import com.wavesplatform.protobuf.ByteStringExt
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
import org.slf4j.LoggerFactory

import java.nio.charset.StandardCharsets
import java.util.concurrent.ScheduledExecutorService
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters.IteratorHasAsScala

class BlockchainGrpcApi(settings: Settings, grpcApiChannel: ManagedChannel, hangScheduler: ScheduledExecutorService)
    extends AutoCloseable
    with ScorexLogging {

  @volatile private var currentObserver = none[RichGrpcObserver[SubscribeRequest, SubscribeEvent]]

  val stream = ConcurrentSubject[Event](MulticastStrategy.publish)(Scheduler(hangScheduler))

  def watchBlockchainUpdates(blockchainUpdatesApiChannel: ManagedChannel, fromHeight: Int, toHeight: Int = 0): Unit = {
    def mkObserver(call: ClientCall[SubscribeRequest, SubscribeEvent]): RichGrpcObserver[SubscribeRequest, SubscribeEvent] =
      new RichGrpcObserver[SubscribeRequest, SubscribeEvent](settings.noDataTimeout, hangScheduler) {
        private val log = LoggerFactory.getLogger(s"DefaultBlockchainApi[$hashCode]")

        override def onReceived(event: SubscribeEvent): Boolean = {
          stream.onNext(Event.Next(event))
          true // TODO
        }

        override def onFailed(error: Throwable): Unit = stream.onNext(Event.Failed(error))

        // We can't obtain IP earlier, it's a netty-grpc limitation
        override def onReady(): Unit = {
          // This doesn't work for streams :(
          // log.debug("Ready to receive from {}", call.socketAddressStr)
        }

        override def onClosedByRemotePart(): Unit = {
          log.error("Unexpected onCompleted by a remote part")
          stream.onNext(Event.Closed)
        }
      }

    val call = blockchainUpdatesApiChannel.newCall(BlockchainUpdatesApiGrpc.METHOD_SUBSCRIBE, CallOptions.DEFAULT)

    val observer = mkObserver(call)
    currentObserver = observer.some

    log.info("Start receiving updates from {}", fromHeight)
    ClientCalls.asyncServerStreamingCall(call, SubscribeRequest(fromHeight = fromHeight, toHeight = toHeight), observer.underlying)
  }

  def getCurrentBlockchainHeight(): Int =
    ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_CURRENT_HEIGHT, CallOptions.DEFAULT),
      Empty()
    )

  def getAccountDataEntries(address: Address): Seq[DataEntry[_]] = {
    ClientCalls
      .blockingServerStreamingCall(
        grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_DATA_ENTRIES, CallOptions.DEFAULT),
        DataRequest(toPb(address))
      )
      .asScala
      .flatMap(_.entry)
      .map(toVanillaDataEntry)
      .toSeq
  }

  def getAccountScript(address: Address): Option[AccountScriptInfo] = {
    val x = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_SCRIPT, CallOptions.DEFAULT),
      AccountRequest(toPb(address))
    )

    toVanillaScript(x.scriptBytes).map { script =>
      AccountScriptInfo(
        publicKey = EmptyPublicKey,
        script = script, // Only this field matters in Ride Runner, see MutableBlockchain.accountScript
        verifierComplexity = x.complexity,
        complexitiesByEstimator = Map.empty // TODO or not?
      )
    }
  }

  /** @return
    *   (header, VRF)
    */
  def getBlockHeader(height: Int): Option[(SignedBlockHeader, ByteStr)] = {
    val x = ClientCalls.blockingUnaryCall(
      grpcApiChannel.newCall(BlocksApiGrpc.METHOD_GET_BLOCK, CallOptions.DEFAULT),
      BlockRequest(request = BlockRequest.Request.Height(height))
    )

    x.block.flatMap(_.header).map { header =>
      // TODO toVanilla
      val signedHeader = SignedBlockHeader(
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
        signature = x.block.fold(ByteString.EMPTY)(_.signature).toByteStr
      )

      (signedHeader, ByteStr.empty) // TODO It seems VRF only from REST API
    }
  }

  def getAssetDescription(asset: Asset.IssuedAsset): Option[AssetDescription] =
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
          lastUpdatedAt = Height(1), // Not used in Ride
          script = for {
            pbScript <- x.script
            script   <- toVanillaScript(pbScript.scriptBytes)
          } yield AssetScriptInfo(script, pbScript.complexity),
          sponsorship = x.sponsorship,
          nft = false // TODO
        )
      )
    } catch {
      case x: StatusException if x.getStatus == Status.NOT_FOUND => None
      case x                                                     => throw x
    }

  def resolveAlias(alias: Alias): Option[Address] = {
    try {
      val x = ClientCalls.blockingUnaryCall(
        grpcApiChannel.newCall(AccountsApiGrpc.METHOD_RESOLVE_ALIAS, CallOptions.DEFAULT),
        alias.toString // TODO ???
      )

      Address.fromBytes(x.toByteArray).toOption
    } catch {
      case e: StatusException if e.getStatus == Status.INVALID_ARGUMENT => None
      case x                                                            => throw x
    }
  }

  def getBalances(address: Address): Portfolio = {
    val xs = ClientCalls.blockingServerStreamingCall(
      grpcApiChannel.newCall(AccountsApiGrpc.METHOD_GET_BALANCES, CallOptions.DEFAULT),
      BalancesRequest(toPb(address))
    )

    xs.asScala.foldLeft(Portfolio.empty) { case (r, x) =>
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
  }

  override def close(): Unit = {
    currentObserver.foreach(_.close())
    stream.onComplete()
  }

  private def toPb(address: Address): ByteString = UnsafeByteOperations.unsafeWrap(address.bytes)

}

object BlockchainGrpcApi {
  case class Settings(noDataTimeout: FiniteDuration)

  sealed trait Event extends Product with Serializable
  object Event {
    case class Next(event: SubscribeEvent) extends Event
    case class Failed(error: Throwable)    extends Event
    case object Closed                     extends Event
  }
}
