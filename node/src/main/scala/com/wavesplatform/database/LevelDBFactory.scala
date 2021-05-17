package com.wavesplatform.database

import com.wavesplatform.utils.ScorexLogging
import org.iq80.leveldb.DBFactory

import scala.util.Try

object LevelDBFactory extends ScorexLogging {
  private[this] val jnaFactory  = "com.wavesplatform.database.jna.LevelDBJNADBFactory"
  private[this] val javaFactory = "org.iq80.leveldb.impl.Iq80DBFactory"

  lazy val factory: DBFactory = {
    val isTesting       = sys.props.get("sbt-testing").isDefined
    val nativeFactories = if (isTesting) List.empty else List(jnaFactory)
    val allFactories    = nativeFactories :+ javaFactory

    val pairs = for {
      loader      <- List(ClassLoader.getSystemClassLoader, getClass.getClassLoader).view
      factoryName <- allFactories
      factory     <- Try(loader.loadClass(factoryName).getConstructor().newInstance().asInstanceOf[DBFactory]).toOption
    } yield (factoryName, factory)

    val (fName, factory) = pairs.headOption.getOrElse(throw new Exception(s"Could not load any of the factory classes: $allFactories"))
    if (fName == javaFactory) log.warn("Using the pure java LevelDB implementation which is still experimental")
    else log.info(s"Loaded $fName with $factory")
    factory
  }
}
