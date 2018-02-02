package com.wavesplatform.db

import org.iq80.leveldb.DBFactory
import scorex.utils.ScorexLogging

object LevelDBFactory extends ScorexLogging {
  private val factory_names = "org.fusesource.leveldbjni.JniDBFactory, org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = load

  private def load: DBFactory = {
    val testing = sys.props.get("sbt-testing")
    val loaders = Seq(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader)

    val names = if (testing.isDefined) Seq("org.iq80.leveldb.impl.Iq80DBFactory")
    else Seq("org.fusesource.leveldbjni.JniDBFactory", "org.iq80.leveldb.impl.Iq80DBFactory")

    val pairs = names.flatMap(x => loaders.map(y => (x, y)))

    val f = pairs.flatMap { case (name, loader) =>
      try {
        val c = loader.loadClass(name).newInstance().asInstanceOf[DBFactory]
        log.trace(s"Loaded ${c.getClass.getName} with $loader")
        Some(c)
      } catch {
        case _: Throwable =>
          None
      }
    }.headOption.getOrElse(throw new Exception(s"Could not load any of the factory classes: $factory_names"))

    if (f.getClass.getName == "org.iq80.leveldb.impl.Iq80DBFactory") {
      log.warn("Using the pure java LevelDB implementation which is still experimental")
    }
    f
  }
}
