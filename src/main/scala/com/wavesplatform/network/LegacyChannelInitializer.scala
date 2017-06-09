package com.wavesplatform.network

import io.netty.channel.socket.SocketChannel
import io.netty.channel.{Channel, ChannelInitializer}
import io.netty.handler.codec.{LengthFieldBasedFrameDecoder, LengthFieldPrepender}
import io.netty.util.concurrent.EventExecutorGroup
import scorex.transaction.History

import scala.concurrent.duration.FiniteDuration

class LegacyChannelInitializer(
    syncTimeout: FiniteDuration,
    history: History,
    peerDatabase: PeerDatabase,
    handshakeHandler: HandshakeHandler,
    discardingHandler: DiscardingHandler,
    messageCodec: MessageCodec,
    utxPoolSynchronizer: UtxPoolSynchronizer,
    scoreObserver: RemoteScoreObserver,
    localScorePublisher: LocalScorePublisher,
    coordinator: Coordinator,
    coordinatorExecutor: EventExecutorGroup,
    blacklist: Channel => Unit) extends ChannelInitializer[SocketChannel] {
  override def initChannel(ch: SocketChannel): Unit =
  ch.pipeline()
    .addLast(
      new HandshakeDecoder,
      new HandshakeTimeoutHandler,
      handshakeHandler,
      new LengthFieldPrepender(4),
      new LengthFieldBasedFrameDecoder(1024*1024, 0, 4, 0, 4),
      new LegacyFrameCodec,
      discardingHandler,
      messageCodec,
      new PeerSynchronizer(peerDatabase),
      new ExtensionSignaturesLoader(syncTimeout, blacklist),
      new ExtensionBlocksLoader(history, syncTimeout, blacklist),
      new OptimisticExtensionLoader,
      utxPoolSynchronizer,
      scoreObserver,
      localScorePublisher)
  .addLast(coordinatorExecutor, coordinator)
}
