package com.wavesplatform.db

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DBFactory

import scala.util.Try

object LevelDBFactory extends ScorexLogging {
  private val nativeFactory = "org.fusesource.leveldbjni.JniDBFactory"
  private val javaFactory   = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val pairs = for {
      loader      <- List(ClassLoader.getSystemClassLoader, this.getClass.getClassLoader).view
      factoryName <- List(nativeFactory, javaFactory)
      factory     <- Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory]).toOption
    } yield (factoryName, factory)

    val (fName, f) = pairs.headOption.getOrElse(throw new Exception(s"Could not load any of the factory classes: $nativeFactory, $javaFactory"))
    if (fName == javaFactory) log.warn("Using the pure java LevelDB implementation which is still experimental")
    else log.trace(s"Loaded $fName with $f")
    f
  }
}
