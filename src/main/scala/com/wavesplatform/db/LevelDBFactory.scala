package com.wavesplatform.db

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DBFactory

object LevelDBFactory extends ScorexLogging {
  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = load

  private def load: DBFactory = {
    val testing = sys.props.get("sbt-testing")
    val loaders = Seq(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader)

    val names =
      if (testing.isDefined) Seq(javaFactory)
      else Seq(nativeFactory, javaFactory)

    val pairs = names.flatMap(x => loaders.map(y => (x, y)))

    val f = pairs.view
      .flatMap {
        case (name, loader) =>
          try {
            val c = loader.loadClass(name).getConstructor().newInstance().asInstanceOf[DBFactory]
            log.trace(s"Loaded ${c.getClass.getName} with $loader")
            Some(c)
          } catch {
            case _: Throwable =>
              None
          }
      }
      .headOption
      .getOrElse(throw new Exception(s"Could not load any of the factory classes: $nativeFactory, $javaFactory"))

    if (f.getClass.getName == javaFactory) {
      log.warn("Using the pure java LevelDB implementation which is still experimental")
    }
    f
  }
}
