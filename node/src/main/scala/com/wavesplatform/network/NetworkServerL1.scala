package com.wavesplatform.network

import com.wavesplatform.settings.{Constants, WavesSettings}
import com.wavesplatform.transaction.LastBlockInfo
import io.netty.channel.{Channel, ChannelHandlerAdapter}
import io.netty.channel.group.ChannelGroup
import monix.reactive.Observable

import java.util.concurrent.ConcurrentHashMap

object NetworkServerL1 {

  def apply(
      settings: WavesSettings,
      lastBlockInfos: Observable[LastBlockInfo],
      historyReplier: HistoryReplierL1,
      peerDatabase: PeerDatabase,
      messageObserver: MessageObserverL1,
      allChannels: ChannelGroup,
      peerInfo: ConcurrentHashMap[Channel, PeerInfo]
  ): NetworkServer = {
    val applicationName = Constants.ApplicationName + settings.blockchainSettings.addressSchemeCharacter

    val peerSynchronizer = if (settings.networkSettings.enablePeersExchange) {
      new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval)
    } else PeerSynchronizer.Disabled

    val protocolSpecificPipeline: Seq[ChannelHandlerAdapter] =
      Seq(
        new LegacyFrameCodecL1(peerDatabase, settings.networkSettings.receivedTxsCacheTimeout),
        new TrafficWatcher,
        new DiscardingHandler(lastBlockInfos.map(_.ready), settings.enableLightMode),
        new MessageCodecL1(peerDatabase),
        new TrafficLoggerL1(settings.networkSettings.trafficLogger),
        peerSynchronizer,
        historyReplier,
        messageObserver
      )

    NetworkServer(
      applicationName,
      settings.networkSettings,
      peerDatabase,
      allChannels,
      peerInfo,
      protocolSpecificPipeline,
    )
  }
}
