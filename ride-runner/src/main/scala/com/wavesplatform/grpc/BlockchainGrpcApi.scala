package com.wavesplatform.grpc

import cats.syntax.option.*
import com.google.protobuf.empty.Empty
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.grpc.{AccountRequest, AccountsApiGrpc, BlockRequest, BlocksApiGrpc, DataRequest}
import com.wavesplatform.block.{BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.events.api.grpc.protobuf.*
import com.wavesplatform.grpc.BlockchainGrpcApi.{Event, Settings}
import com.wavesplatform.grpc.observers.RichGrpcObserver
import com.wavesplatform.protobuf.transaction.PBTransactions.{toVanillaDataEntry, toVanillaScript}
import com.wavesplatform.ride.input.EmptyPublicKey
import com.wavesplatform.state.{AccountScriptInfo, DataEntry}
import com.wavesplatform.utils.ScorexLogging
import io.grpc.stub.ClientCalls
import io.grpc.{CallOptions, ClientCall, ManagedChannel}
import monix.execution.Scheduler
import monix.reactive.MulticastStrategy
import monix.reactive.subjects.ConcurrentSubject
import org.slf4j.LoggerFactory

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
          reference = ByteStr(header.reference.toByteArray),
          baseTarget = header.baseTarget,
          generationSignature = ByteStr(header.generationSignature.toByteArray),
          generator = PublicKey(header.generator.toByteArray),
          featureVotes = header.featureVotes.map(_.toShort),
          rewardVote = header.rewardVote,
          transactionsRoot = ByteStr(header.transactionsRoot.toByteArray)
        ),
        signature = ByteStr(x.block.fold(ByteString.EMPTY)(_.signature).toByteArray)
      )

      (signedHeader, ByteStr.empty) // TODO It seems VRF only from REST API
    }
  }

  override def close(): Unit = {
    currentObserver.foreach(_.close())
    stream.onComplete()
  }

  private def toPb(address: Address) = UnsafeByteOperations.unsafeWrap(address.bytes)

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
