package scorex.crypto.ads.merkle

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import org.slf4j.LoggerFactory
import scorex.crypto.CryptographicHash.Digest

import scala.util.{Failure, Success, Try}

class MapDBStorage(fileName: String, levels: Int) extends Storage {

  import Storage._

  private val log = LoggerFactory.getLogger(this.getClass)

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

  override def set(key: Key, value: Digest): Unit = {
    val map = maps(key._1.asInstanceOf[Int])
    val t = Try {
      map.put(key._2, value)
    }
    if (t.isFailure) {
      log.warn("Failed to set key:" + key)
    }
  }


  override def commit(): Unit = {
    dbs.map(_.commit())
  }

  override def close(): Unit = {
    commit()
    dbs.map(_.close())
  }

  override def get(key: Key): Option[Digest] = {
    Try {
      maps(key._1).get(key._2)
    } match {
      case Failure(e) =>
        if (key._1 == 0) {
          log.debug("Enable to load key for level 0: " + key)
        }
        None
      case Success(v) =>
        Option(v)
    }
  }

}
