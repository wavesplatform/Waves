package com.wavesplatform

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.test.*
import kamon.Kamon

import java.nio.file.Files
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

class LoadApplicationConfigSpec extends FreeSpec {
  "loadApplicationConfig starts no Kamon reporter threads, which are non-daemon and would keep a tool's JVM alive" in {
    val dir  = Files.createTempDirectory("load-app-config")
    val conf = dir.resolve("kamon-on.conf")
    // TESTNET keeps AddressScheme at the test default 'T' for suites running in parallel.
    Files.writeString(conf, s"""waves { directory = "$dir", blockchain.type = TESTNET, kamon.enable = yes }""")

    val addressScheme = AddressScheme.current
    val directoryProp = Option(System.getProperty("waves.directory"))
    try {
      val config      = Application.loadApplicationConfig(Some(conf.toFile)).config
      val moduleNames = config.getConfig("kamon.modules").root().keySet().asScala.map(m => config.getString(s"kamon.modules.$m.name"))
      val leaked      = Thread.getAllStackTraces.keySet().asScala.filter(t => moduleNames(t.getName))
      try leaked.map(_.getName) shouldBe empty
      finally if (leaked.nonEmpty) Await.ready(Kamon.stop(), 30.seconds)
    } finally {
      AddressScheme.current = addressScheme
      directoryProp.fold(System.clearProperty("waves.directory"))(System.setProperty("waves.directory", _))
      Files.delete(conf)
      Files.delete(dir)
    }
  }
}
