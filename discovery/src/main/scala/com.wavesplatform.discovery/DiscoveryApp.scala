package com.wavesplatform.discovery

import java.net.InetSocketAddress
import java.util
import java.util.concurrent.TimeUnit

import com.wavesplatform.Version
import com.wavesplatform.network.{BasicMessagesRepo, BlockForged, BlockMessageSpec, GetBlock, GetBlockSpec, GetPeers, GetPeersSpec, GetSignatures, GetSignaturesSpec, Handshake, KnownPeers, LocalScoreChanged, Message, MicroBlockInv, MicroBlockInvMessageSpec, MicroBlockRequest, MicroBlockRequestMessageSpec, MicroBlockResponse, MicroBlockResponseMessageSpec, PeerDatabase, PeersSpec, PipelineInitializer, RawBytes, ScoreMessageSpec, Signatures, SignaturesSpec, id}
import com.wavesplatform.settings.Constants
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.ByteBuf
import io.netty.buffer.Unpooled.wrappedBuffer
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel.{ChannelHandlerContext, ChannelInboundHandlerAdapter, EventLoopGroup}
import io.netty.channel.socket.SocketChannel
import io.netty.handler.codec._
import scorex.crypto.hash.FastCryptographicHash
import scorex.network.message.Message.ChecksumLength
import scorex.network.message.MessageSpec
import scorex.utils.ScorexLogging

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration
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

class MessageHandler(handler: PartialFunction[Any, Unit]) extends ChannelInboundHandlerAdapter{
  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any): Unit = {
    handler(msg)
//    msg match {
//      case hs: Handshake => {
//        ctx.writeAndFlush(GetPeers)
//      }
//      case KnownPeers(peers) => {
//        ctx.close()
//      }
//      case _ =>
//    }
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
  import io.netty.channel.nio.NioEventLoopGroup

  implicit val workerGroup = new NioEventLoopGroup


  import io.netty.channel.socket.nio.NioSocketChannel


  implicit val ec: ExecutionContext = ExecutionContext.global

  def getPeersFromNode(address: InetSocketAddress)(implicit eventLoopGroup: EventLoopGroup): scala.concurrent.Future[Seq[InetSocketAddress]] = Future {
    var peers: Seq[InetSocketAddress] = Seq.empty

    new Bootstrap()
      .group(workerGroup)
      .channel(classOf[NioSocketChannel])
      .handler(new PipelineInitializer[SocketChannel](Seq(
        new HandshakeHandler(),
        new LengthFieldPrepender(4),
        new LengthFieldBasedFrameDecoder(100 * 1024 * 1024, 0, 4, 0, 4),
        new LegacyFrameCodec(),
        new MessageCodec(),
        new MessageHandler({
          case hs: Handshake => println("HS!")
          case _ =>
        })
      )))
      .remoteAddress(address.getAddress, address.getPort)
      .connect().channel().closeFuture().sync()

    peers
  }

  var results = Await.result(getPeersFromNode(Settings.default.initialPeers.head), FiniteDuration(1, TimeUnit.MINUTES))
  print(results)
  workerGroup.shutdownGracefully()



  //  implicit val actorSystem = ActorSystem("akka-system")
//  implicit val flowMaterializer = ActorMaterializer()
//
//  val interface = "localhost"
//  val port = 8080
//
//  val source: Source[Message, _] = Source.single(0).map(_ => {
//    println("Connected")
//    TextMessage("!!!!!")
//  })
//
//  val sink: Sink[Message, _] =  Sink.ignore
//
//  val echoService: Flow[Message, Message, _] =  Flow[Message].map {
//    case TextMessage.Strict(txt) => TextMessage("ECHO: " + txt)
//    case _ => TextMessage("Message type unsupported")
//  }
//
//  import akka.http.scaladsl.server.Directives._
//
//  val route = get {
//    pathEndOrSingleSlash {
//      complete("Welcome to websocket server")
//    }
//  } ~ get {
//    path("ws-echo") {
//      handleWebSocketMessages(echoService)
//    }
//  }
//
//
//  val binding = Http().bindAndHandle(route, interface, port)
//  println(s"Server is now online at http://$interface:$port\nPress RETURN to stop...")
//  StdIn.readLine()
//
//  import actorSystem.dispatcher
//
//  binding.flatMap(_.unbind()).onComplete(_ => actorSystem.terminate())
//  println("Server is down...")
}
