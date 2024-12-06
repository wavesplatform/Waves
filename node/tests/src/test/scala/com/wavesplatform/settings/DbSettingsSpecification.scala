package com.wavesplatform.settings

import com.typesafe.config.ConfigFactory
import com.wavesplatform.test.FlatSpec
import pureconfig.ConfigSource
import com.typesafe.config.ConfigException.BadValue

class DbSettingsSpecification extends FlatSpec {
  "SizeInBytes" should "should successfully read bytes values" in {
    val config        = loadConfig(ConfigFactory.parseString("size-in-bytes-value = 512M"))
    val actualValue   = ConfigSource.fromConfig(config).at("size-in-bytes-value").loadOrThrow[SizeInBytes]
    val expectedValue = SizeInBytes(512L * 1024 * 1024)
    actualValue should be(expectedValue)
  }

  "SizeInBytes" should "should fail on invalid values" in {
    val config = loadConfig(ConfigFactory.parseString("size-in-bytes-value = 512X"))
    assertThrows[BadValue] {
      ConfigSource.fromConfig(config).at("size-in-bytes-value").loadOrThrow[SizeInBytes]
    }
  }

  "DbSettingsSpecification" should "read values from config" in {
    val config = loadConfig(ConfigFactory.parseString("""waves.db {
                                                        |  directory = "/data"
                                                        |  store-transactions-by-address = true
                                                        |  store-lease-states-by-address = true
                                                        |  store-invoke-script-results = true
                                                        |  store-state-hashes = false
                                                        |  max-cache-size = 100000
                                                        |  max-rollback-depth = 2000
                                                        |  cleanup-interval = 500
                                                        |  rocksdb {
                                                        |    main-cache-size = 512M
                                                        |    tx-cache-size = 16M
                                                        |    tx-meta-cache-size = 16M
                                                        |    tx-snapshot-cache-size = 16M
                                                        |    api-cache-size=16M
                                                        |    write-buffer-size = 128M
                                                        |    enable-statistics = false
                                                        |    allow-mmap-reads = off
                                                        |    parallelism = 2
                                                        |    max-open-files = 100
                                                        |  }
                                                        |}""".stripMargin))

    val actualDbSettings = ConfigSource.fromConfig(config).at("waves.db").loadOrThrow[DBSettings]

    val expectedDbSettings: DBSettings = DBSettings(
      directory = "/data",
      storeTransactionsByAddress = true,
      storeLeaseStatesByAddress = true,
      storeInvokeScriptResults = true,
      storeStateHashes = false,
      maxCacheSize = 100000,
      maxRollbackDepth = 2000,
      cleanupInterval = Some(500),
      rocksdb = RocksDBSettings(
        mainCacheSize = SizeInBytes(512L * 1024 * 1024),
        txCacheSize = SizeInBytes(16L * 1024 * 1024),
        txMetaCacheSize = SizeInBytes(16L * 1024 * 1024),
        txSnapshotCacheSize = SizeInBytes(16L * 1024 * 1024),
        apiCacheSize = SizeInBytes(16L * 1024 * 1024),
        writeBufferSize = SizeInBytes(128L * 1024 * 1024),
        enableStatistics = false,
        allowMmapReads = false,
        parallelism = 2,
        maxOpenFiles = 100
      )
    )

    actualDbSettings should be(expectedDbSettings)
  }
}
