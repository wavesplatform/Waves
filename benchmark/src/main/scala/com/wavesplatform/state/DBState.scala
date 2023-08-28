package com.wavesplatform.state

import java.io.File

import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.database.{RDB, RocksDBWriter}
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.smart.WavesEnvironment
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Coeval
import org.openjdk.jmh.annotations.{Param, Scope, State, TearDown}

@State(Scope.Benchmark)
abstract class DBState extends ScorexLogging {
  @Param(Array("waves.conf"))
  var configFile = ""

  lazy val settings: WavesSettings = Application.loadApplicationConfig(Some(new File(configFile)).filter(_.exists()))

  lazy val rdb: RDB = RDB.open(settings.dbSettings)

  lazy val rocksDBWriter: RocksDBWriter =
    new RocksDBWriter(
      rdb,
      settings.blockchainSettings,
      settings.dbSettings.copy(maxCacheSize = 1),
      settings.enableLightMode
    )

  AddressScheme.current = new AddressScheme { override val chainId: Byte = 'W' }

  lazy val environment = new WavesEnvironment(
    AddressScheme.current.chainId,
    Coeval.raiseError(new NotImplementedError("`tx` is not implemented")),
    Coeval(rocksDBWriter.height),
    rocksDBWriter,
    null,
    DirectiveSet.contractDirectiveSet,
    ByteStr.empty
  )

  @TearDown
  def close(): Unit = {
    rdb.close()
  }
}
