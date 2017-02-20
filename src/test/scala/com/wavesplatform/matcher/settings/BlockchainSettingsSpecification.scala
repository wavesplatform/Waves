package com.wavesplatform.matcher.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.settings.{BlockchainSettings, GenesisTransactionSettings}
import org.scalatest.{FlatSpec, Matchers}

class BlockchainSettingsSpecification extends FlatSpec with Matchers {
  "BlockchainSettings" should "read custom values" in {
    val config = ConfigFactory.parseString(
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
        |      }
        |      genesis {
        |        timestamp: 1460678400000
        |        signature: "BASE58BLOCKSIGNATURE"
        |        initial-balance: 100000000000000
        |        transactions = [
        |          {recipient: "BASE58ADDRESS1", amount: 50000000000001},
        |          {recipient: "BASE58ADDRESS2", amount: 49999999999999}
        |        ]
        |      }
        |    }
        |  }
        |}
      """.stripMargin).resolve()
    val settings = BlockchainSettings.fromConfig(config)

    settings.file should be("/waves/data/blockchain.dat")
    settings.addressSchemeCharacter should be('C')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(2)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(3)
    settings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight should be(4)
    settings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp should be(5)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(6)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(7)
    settings.functionalitySettings.allowBurnTransactionAfterTimestamp should be(8)
    settings.functionalitySettings.requirePaymentUniqueId should be(9)
    settings.genesisSettings.timestamp should be (1460678400000L)
    settings.genesisSettings.signature should be ("BASE58BLOCKSIGNATURE")
    settings.genesisSettings.initialBalance should be (100000000000000L)
    settings.genesisSettings.transactions.size should be (2)
    settings.genesisSettings.transactions.head should be (GenesisTransactionSettings("BASE58ADDRESS1", 50000000000001L))
    settings.genesisSettings.transactions.tail.head should be (GenesisTransactionSettings("BASE58ADDRESS2", 49999999999999L))
  }

  it should "read testnet settings" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  blockchain {
        |    file: ${waves.directory}"/data/blockchain.dat"
        |    type: TESTNET
        |  }
        |}
      """.stripMargin).resolve()
    val settings = BlockchainSettings.fromConfig(config)

    settings.file should be("/waves/data/blockchain.dat")
    settings.addressSchemeCharacter should be('T')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1477958400000L)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(1477958400000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1477958400000L)
    settings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight should be(Long.MinValue)
    settings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp should be(Long.MinValue)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(Long.MinValue)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1479416400000L)
    settings.functionalitySettings.allowBurnTransactionAfterTimestamp should be(1481110521000L)
    settings.functionalitySettings.requirePaymentUniqueId should be(1485942685000L)
    settings.genesisSettings.timestamp should be (1478000000000L)
    settings.genesisSettings.signature should be ("5uqnLK3Z9eiot6FyYBfwUnbyid3abicQbAZjz38GQ1Q8XigQMxTK4C1zNkqS1SVw7FqSidbZKxWAKLVoEsp4nNqa")
    settings.genesisSettings.initialBalance should be (10000000000000000L)
    settings.genesisSettings.transactions.size should be (5)
    settings.genesisSettings.transactions.head should be (GenesisTransactionSettings("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8", 400000000000000L))
    settings.genesisSettings.transactions.tail.head should be (GenesisTransactionSettings("3NBVqYXrapgJP9atQccdBPAgJPwHDKkh6A8", 200000000000000L))
    settings.genesisSettings.transactions.tail.tail.head should be (GenesisTransactionSettings("3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh", 200000000000000L))
    settings.genesisSettings.transactions.tail.tail.tail.head should be (GenesisTransactionSettings("3NCBMxgdghg4tUhEEffSXy11L6hUi6fcBpd", 200000000000000L))
    settings.genesisSettings.transactions.tail.tail.tail.tail.head should be (GenesisTransactionSettings("3N18z4B8kyyQ96PhN5eyhCAbg4j49CgwZJx", 9000000000000000L))
  }

  it should "read mainnet settings" in {
    val config = ConfigFactory.parseString(
      """
        |waves {
        |  directory: "/waves"
        |  blockchain {
        |    file: ${waves.directory}"/data/blockchain.dat"
        |    type: MAINNET
        |  }
        |}
      """.stripMargin).resolve()
    val settings = BlockchainSettings.fromConfig(config)

    settings.file should be("/waves/data/blockchain.dat")
    settings.addressSchemeCharacter should be('W')
    settings.functionalitySettings.allowTemporaryNegativeUntil should be(1479168000000L)
    settings.functionalitySettings.allowInvalidPaymentTransactionsByTimestamp should be(1479168000000L)
    settings.functionalitySettings.requireSortedTransactionsAfter should be(1479168000000L)
    settings.functionalitySettings.generatingBalanceDepthFrom50To1000AfterHeight should be(232000L)
    settings.functionalitySettings.minimalGeneratingBalanceAfterTimestamp should be(1479168000000L)
    settings.functionalitySettings.allowTransactionsFromFutureUntil should be(1479168000000L)
    settings.functionalitySettings.allowUnissuedAssetsUntil should be(1479416400000L)
    settings.functionalitySettings.allowBurnTransactionAfterTimestamp should be(1482233593000L)
    settings.functionalitySettings.requirePaymentUniqueId should be(1488361885000L)
    settings.genesisSettings.timestamp should be (1465742577614L)
    settings.genesisSettings.signature should be ("FSH8eAAzZNqnG8xgTZtz5xuLqXySsXgAjmFEC25hXMbEufiGjqWPnGCZFt6gLiVLJny16ipxRNAkkzjjhqTjBE2")
    settings.genesisSettings.initialBalance should be (10000000000000000L)
    settings.genesisSettings.transactions.size should be (6)
    settings.genesisSettings.transactions.head should be (GenesisTransactionSettings("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ", 9999999500000000L))
    settings.genesisSettings.transactions.tail.head should be (GenesisTransactionSettings("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM", 100000000L))
    settings.genesisSettings.transactions.tail.tail.head should be (GenesisTransactionSettings("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy", 100000000L))
    settings.genesisSettings.transactions.tail.tail.tail.head should be (GenesisTransactionSettings("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF", 100000000L))
    settings.genesisSettings.transactions.tail.tail.tail.tail.head should be (GenesisTransactionSettings("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3", 100000000L))
    settings.genesisSettings.transactions.tail.tail.tail.tail.tail.head should be (GenesisTransactionSettings("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J", 100000000L))
  }
}
