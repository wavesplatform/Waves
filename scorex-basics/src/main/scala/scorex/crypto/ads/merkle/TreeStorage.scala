package scorex.crypto.ads.merkle

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import scorex.crypto.hash.CryptographicHash.Digest
import scorex.storage.Storage
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

@deprecated("Use tree storage from scrypto library", "1.2.2")
class TreeStorage(fileName: String, levels: Int) extends Storage[(Int, Long), Array[Byte]] with ScorexLogging {

  import TreeStorage._

  private val dbs =
    (0 to levels) map { n: Int =>
      DBMaker.fileDB(new File(fileName + n + ".mapDB"))
        .fileMmapEnableIfSupported()
        .closeOnJvmShutdown()
        .checksumEnable()
        .make()
    }

  private val maps: Map[Int, HTreeMap[Long, Digest]] = {
    val t = (0 to levels) map { n: Int =>
      val m: HTreeMap[Long, Digest] = dbs(n).hashMapCreate("map_" + n)
        .keySerializer(Serializer.LONG)
        .valueSerializer(Serializer.BYTE_ARRAY)
        .makeOrGet()
      n -> m
    }
    t.toMap
  }

  override def set(key: Key, value: Digest): Unit = Try {
    maps(key._1.asInstanceOf[Int]).put(key._2, value)
  }.recoverWith { case t: Throwable =>
    log.warn("Failed to set key:" + key, t)
    Failure(t)
  }

  override def commit(): Unit = dbs.foreach(_.commit())

  override def close(): Unit = {
    commit()
    dbs.foreach(_.close())
  }

  override def get(key: Key): Option[Digest] = {
    Try {
      maps(key._1).get(key._2)
    } match {
      case Success(v) =>
        Option(v)

      case Failure(e) =>
        if (key._1 == 0) {
          log.debug("Enable to load key for level 0: " + key)
        }
        None
    }
  }

}

object TreeStorage {
  type Level = Int
  type Position = Long
  type Key = (Level, Position)
  type Value = Digest
}
