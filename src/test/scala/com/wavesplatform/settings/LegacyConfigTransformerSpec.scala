package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FreeSpec, Matchers}

class LegacyConfigTransformerSpec extends FreeSpec with Matchers {
  private val legacyConfig =
    """{
      |  "p2p": {
      |    "myAddress": "0.0.0.0",
      |    "nodeName": "",
      |    "bindAddress": "0.0.0.0",
      |    "port": 6868,
      |    "upnp": false,
      |    "upnpGatewayTimeout": 7000,
      |    "upnpDiscoverTimeout": 3000,
      |    "knownPeers": [
      |      "138.201.152.166:6868",
      |      "138.201.152.165:6868",
      |      "138.201.152.164:6868",
      |      "138.201.152.163:6868",
      |      "34.192.116.209:6868",
      |      "52.8.147.91:6868",
      |      "52.192.108.188:6868",
      |      "52.62.212.53:6868",
      |      "52.67.63.123:6868"
      |    ],
      |    "maxConnections": 30
      |  },
      |  "walletDir": "",
      |  "walletSeed": "",
      |  "walletPassword": "ridethewaves!",
      |  "dataDir": "",
      |  "rpcEnabled": false,
      |  "rpcPort": 6869,
      |  "rpcAddress": "127.0.0.1",
      |  "blockGenerationDelay": 15000,
      |  "historySynchronizerTimeout": 15,
      |  "cors": true,
      |  "maxRollback": 100,
      |  "apiKeyHash": "H6nsiifwYKYEx6YzYD7woP1XCn72RVvx6tC1zjjLXqsu",
      |  "history": "blockchain",
      |  "offlineGeneration": false,
      |  "testnet": false,
      |  "loggingLevel": "info",
      |  "genesisSignature": "FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2",
      |  "checkpoints": {
      |    "publicKey": "7EXnkmJyz1gPfLJwytThcwGwpyfjzFXC3hxBhvVK4EQP"
      |  }
      |}
      |""".stripMargin

  "properly parses custom values from legacy config file" in {
    val legacyConfigFromJson = LegacyConfigTransformer
      .transform(ConfigFactory.parseString(legacyConfig))
      .withFallback(ConfigFactory.parseString(
        """waves {
          |  blockchain.file = ""
          |  network {
          |    file = ""
          |    unrequested-packets-threshold = 100
          |  }
          |  matcher {
          |    enable = false
          |    account = ""
          |    bind-address = ""
          |    port = 0
          |    min-order-fee = 0
          |    order-match-tx-fee = 0
          |    journal-directory = ""
          |    snapshots-directory = ""
          |    snapshots-interval = 10m
          |    max-open-orders = 1000
          |  }
          |  synchronization.score-ttl = 90s
          |  utx {}
          |}
          |""".stripMargin))

    WavesSettings.fromConfig(legacyConfigFromJson)
  }
}
