package com.wavesplatform.matcher

import java.io.File
import java.nio.file.Files.createTempDirectory

import akka.persistence.snapshot.SnapshotStoreSpec
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.TestHelpers.deleteRecursively
import com.wavesplatform.settings.loadConfig

class MatcherSnapshotStoreSpec
    extends SnapshotStoreSpec(loadConfig(parseString(s"waves.matcher.snapshot-store.leveldb-dir = ${createTempDirectory("matcher").toAbsolutePath}"))) {
  protected override def afterAll(): Unit = {
    super.afterAll()
    deleteRecursively(new File(system.settings.config.getString("waves.matcher.snapshot-store.leveldb-dir")).toPath)
  }
}
