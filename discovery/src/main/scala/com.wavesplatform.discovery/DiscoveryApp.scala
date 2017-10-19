package com.wavesplatform.discovery

import java.util
import java.util.concurrent.TimeUnit

import akka.NotUsed
import akka.actor.{Actor, ActorRef, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props, SupervisorStrategy}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.TextMessage
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.{Flow, Sink, Source}
import com.wavesplatform.Version
import com.wavesplatform.discovery.actors.MainActor.WebSocketConnected
import com.wavesplatform.discovery.actors.{IOActor, MainActor}
import com.wavesplatform.network.{BasicMessagesRepo, BlockForged, BlockMessageSpec, GetBlock, GetBlockSpec, GetPeers, GetPeersSpec, GetSignatures, GetSignaturesSpec, Handshake, KnownPeers, LocalScoreChanged, Message, MicroBlockInv, MicroBlockInvMessageSpec, MicroBlockRequest, MicroBlockRequestMessageSpec, MicroBlockResponse, MicroBlockResponseMessageSpec, PeerDatabase, PeersSpec, PipelineInitializer, RawBytes, ScoreMessageSpec, Signatures, SignaturesSpec, id}
import com.wavesplatform.settings.Constants
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled.wrappedBuffer
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter, EventLoopGroup}
import io.netty.handler.codec._
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message.ChecksumLength
import scorex.network.message.MessageSpec
import scorex.utils.ScorexLogging

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
import scala.io.StdIn
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

class LegacyFrameCodec() extends ByteToMessageCodec[RawBytes] with ScorexLogging {
  import LegacyFrameCodec._

  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]) = try {
    require(in.readInt() == Magic, "invalid magic number")

    val code = in.readByte()
    require(messageSpecs.contains(code), s"Unexpected message code $code")

    val spec = messageSpecs(code)
    val length = in.readInt()
    require(length <= spec.maxLength, s"${spec.messageName} length $length exceeds ${spec.maxLength}")

    val dataBytes = new Array[Byte](length)
    if (length > 0) {
      val declaredChecksum = in.readSlice(ChecksumLength)
      in.readBytes(dataBytes)
      val actualChecksum = wrappedBuffer(FastCryptographicHash.hash(dataBytes), 0, ChecksumLength)

      require(declaredChecksum.equals(actualChecksum), "invalid checksum")
      actualChecksum.release()

    }

    out.add(RawBytes(code, dataBytes))
  } catch {
    case NonFatal(e) =>
      log.warn(s"${id(ctx)} Malformed network message", e)
  }

  override def encode(ctx: ChannelHandlerContext, msg: RawBytes, out: ByteBuf) = {
    out.writeInt(Magic)
    out.writeByte(msg.code)
    if (msg.data.length > 0) {
      out.writeInt(msg.data.length)
      out.writeBytes(FastCryptographicHash.hash(msg.data), 0, ChecksumLength)
      out.writeBytes(msg.data)
    } else {
      out.writeInt(0)
    }
  }
}

object LegacyFrameCodec {
  val Magic = 0x12345678

  private val messageSpecs: Map[Byte, MessageSpec[_ <: AnyRef]] =
    BasicMessagesRepo.specs.map(s => s.messageCode -> s).toMap
}

@Sharable
class MessageCodec() extends MessageToMessageCodec[RawBytes, Message] with ScorexLogging {

  private val specs: Map[Byte, MessageSpec[_ <: AnyRef]] = BasicMessagesRepo.specs.map(s => s.messageCode -> s).toMap

  override def encode(ctx: ChannelHandlerContext, msg: Message, out: util.List[AnyRef]) = msg match {
    case LocalScoreChanged(score) => out.add(RawBytes(ScoreMessageSpec.messageCode, ScoreMessageSpec.serializeData(score)))
    case GetPeers => out.add(RawBytes(GetPeersSpec.messageCode, Array[Byte]()))
    case k: KnownPeers => out.add(RawBytes(PeersSpec.messageCode, PeersSpec.serializeData(k)))
    case gs: GetSignatures => out.add(RawBytes(GetSignaturesSpec.messageCode, GetSignaturesSpec.serializeData(gs)))
    case s: Signatures => out.add(RawBytes(SignaturesSpec.messageCode, SignaturesSpec.serializeData(s)))
    case g: GetBlock => out.add(RawBytes(GetBlockSpec.messageCode, GetBlockSpec.serializeData(g)))
    case BlockForged(b) => out.add(RawBytes(BlockMessageSpec.messageCode, b.bytes))
    case m: MicroBlockInv => out.add(RawBytes(MicroBlockInvMessageSpec.messageCode, MicroBlockInvMessageSpec.serializeData(m)))
    case m: MicroBlockRequest => out.add(RawBytes(MicroBlockRequestMessageSpec.messageCode, MicroBlockRequestMessageSpec.serializeData(m)))
    case m: MicroBlockResponse => out.add(RawBytes(MicroBlockResponseMessageSpec.messageCode, MicroBlockResponseMessageSpec.serializeData(m)))
    case r: RawBytes => out.add(r)
  }

  override def decode(ctx: ChannelHandlerContext, msg: RawBytes, out: util.List[AnyRef]): Unit = {
    specs(msg.code).deserializeData(msg.data) match {
      case Success(x) => out.add(x)
      case Failure(e) => println(e.getMessage)
    }
  }
}

class MessageHandler(handler: PartialFunction[(Any, ChannelHandlerContext), Unit]) extends ChannelInboundHandlerAdapter{
  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any): Unit = {
    handler((msg, ctx))
  }
}

class HandshakeHandler() extends ReplayingDecoder[Void] with ScorexLogging {
  private val handshake =
    Handshake(Constants.ApplicationName + Settings.default.chainId, Version.VersionTuple,
      "discovery", new Random().nextLong(), None)

  override def decode(ctx: ChannelHandlerContext, in: ByteBuf, out: util.List[AnyRef]): Unit = {
    out.add(Handshake.decode(in))
    ctx.pipeline().remove(this)
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    ctx.writeAndFlush(handshake.encode(ctx.alloc().buffer()))
  }
}


object DiscoveryApp extends App {

  implicit val system = ActorSystem("Default")
  implicit val flowMaterializer = ActorMaterializer()

  val mainActor = system.actorOf(Props[MainActor], name = "main")

  mainActor ! MainActor.Peers(Settings.default.initialPeers.toSet)

  implicit val ec: ExecutionContext = ExecutionContext.global
  system.scheduler.schedule(FiniteDuration(1, TimeUnit.SECONDS),FiniteDuration(1, TimeUnit.SECONDS), mainActor, MainActor.Discover)

  //var results = Await.result(getPeersFromNode(Settings.default.initialPeers(2)), FiniteDuration(1, TimeUnit.MINUTES))
  //print(results)

  //workerGroup.shutdownGracefully()

  val interface = "localhost"
  val port = 8080

//
  import akka.http.scaladsl.server.Directives._
//
  val route = get {
    pathEndOrSingleSlash {
      complete("Welcome to websocket server")
    }
  } ~ get {
    path("ws-echo") {

      val sink: Sink[akka.http.scaladsl.model.ws.Message, _] =  Sink.ignore

      val source: Source[akka.http.scaladsl.model.ws.Message, NotUsed] =
        Source.actorRef[String](1, OverflowStrategy.dropTail)
          .mapMaterializedValue { actor =>
            mainActor ! WebSocketConnected(actor)
            NotUsed
          }.map(
          // transform domain message to web socket message
          (outMsg: String) => TextMessage(outMsg))

      handleWebSocketMessages(Flow.fromSinkAndSource(sink, source))
    }
  }
//
//
  val binding = Http().bindAndHandle(route, interface, port)
  println(s"Server is now online at http://$interface:$port\nPress RETURN to stop...")
  StdIn.readLine()
//
//  import actorSystem.dispatcher
//
//  binding.flatMap(_.unbind()).onComplete(_ => actorSystem.terminate())
//  println("Server is down...")
}
