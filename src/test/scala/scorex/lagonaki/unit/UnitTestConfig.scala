package scorex.lagonaki.unit

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}

trait UnitTestConfig {
  val config: Config = ConfigFactory.parseString(
    """
      |waves {
      |  directory: "/waves"
      |  blockchain {
      |    file: ${waves.directory}"/data/blockchain.dat"
      |    type: CUSTOM
      |    custom {
      |      address-scheme-character: "C"
      |      functionality {
      |        allow-temporary-negative-until: 1
      |        allow-invalid-payment-transactions-by-timestamp: 2
      |        require-sorted-transactions-after: 3
      |        generation-balance-depth-from-50-to-1000-after-height: 4
      |        minimal-generating-balance-after: 5
      |        allow-transactions-from-future-until: 6
      |        allow-unissued-assets-until: 7
      |        allow-burn-transaction-after: 8
      |        require-payment-unique-id-after: 9
      |        allow-lease-transaction-after: 10
      |        allow-exchange-transaction-after: 11
      |        allow-invalid-reissue-in-same-block-until-timestamp: 12
      |        allow-createalias-transaction-after: 13
      |        allow-multiple-lease-cancel-transaction-until-timestamp: 14
      |        reset-effective-balances-at-height: 15
      |      }
      |      genesis {
      |        timestamp: 1460678400000
      |        signature: "BASE58BLOCKSIGNATURE"
      |        initial-balance: 100000000000000
      |        initial-base-target = 153722867
      |        average-block-delay = 60s
      |        transactions = [
      |          {recipient: "BASE58ADDRESS1", amount: 50000000000001},
      |          {recipient: "BASE58ADDRESS2", amount: 49999999999999}
      |        ]
      |      }
      |    }
      |  }
      |}
    """.stripMargin).resolve()
  val blockchainSettings: BlockchainSettings = BlockchainSettings.fromConfig(config)
  val wavesSettings = WavesSettings.fromConfig(ConfigFactory.load())
}
