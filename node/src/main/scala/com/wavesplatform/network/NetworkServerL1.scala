package com.wavesplatform.network

import com.wavesplatform.settings.{Constants, WavesSettings}
import com.wavesplatform.transaction.LastBlockInfo
import io.netty.channel.Channel
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

    def peerSynchronizer = if (settings.networkSettings.enablePeersExchange) {
      new PeerSynchronizer(peerDatabase, settings.networkSettings.peersBroadcastInterval)
    } else PeerSynchronizer.Disabled
    val trafficWatcher    = new TrafficWatcher
    val discardingHandler = new DiscardingHandler(lastBlockInfos.map(_.ready), settings.enableLightMode)
    val messageCodec      = new MessageCodecL1(peerDatabase)
    val trafficLogger     = new TrafficLoggerL1(settings.networkSettings.trafficLogger)

    NetworkServer(
      applicationName,
      settings.networkSettings,
      peerDatabase,
      allChannels,
      peerInfo,
      Seq(
        new LegacyFrameCodecL1(peerDatabase, settings.networkSettings.receivedTxsCacheTimeout),
        trafficWatcher,
        discardingHandler,
        messageCodec,
        trafficLogger,
        peerSynchronizer,
        historyReplier,
        messageObserver
      )
    )
  }
}
