package com.wavesplatform.test

import java.nio.file.{Files, Path}

import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.database.LevelDBFactory
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.{NTPTime, TestHelpers}
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait MiniNode extends BeforeAndAfterAll with NTPTime { this: Suite =>
  lazy val path: Path = Files.createTempDirectory("lvl-temp").toAbsolutePath
  lazy val db: DB     = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))

  def settings: WavesSettings

  def genesis(recipient: Address, amount: Long): GenesisTransaction =
    GenesisTransaction.create(recipient, amount, ntpTime.getTimestamp()).explicitGet()

  override protected def afterAll(): Unit = {
    super.afterAll()
    db.close()
    TestHelpers.deleteRecursively(path)
  }
}
