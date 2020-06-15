package com.wavesplatform.state

import java.io.File

import com.wavesplatform.Application
import com.wavesplatform.database.{LevelDBWriter, openDB}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DB
import org.openjdk.jmh.annotations.{Param, Scope, State, TearDown}

@State(Scope.Benchmark)
abstract class DBState extends ScorexLogging {
  @Param(Array("waves.conf"))
  var configFile = ""

  lazy val settings: WavesSettings = Application.loadApplicationConfig(Some(new File(configFile)).filter(_.exists()))

  lazy val db: DB = openDB(settings.dbSettings.directory)

  lazy val levelDBWriter: LevelDBWriter = LevelDBWriter.readOnly(db, settings)

  @TearDown
  def close(): Unit = {
    db.close()
  }
}
