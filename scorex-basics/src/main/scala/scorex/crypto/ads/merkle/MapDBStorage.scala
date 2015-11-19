package scorex.crypto.ads.merkle

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import org.slf4j.LoggerFactory
import scorex.crypto.CryptographicHash.Digest

import scala.util.{Success, Failure, Try}

class MapDBStorage(file: File) extends Storage {

  import Storage._

  private val log = LoggerFactory.getLogger(this.getClass)

  private val db = DBMaker.fileDB(file)
    .fileMmapEnableIfSupported()
    .closeOnJvmShutdown()
    .checksumEnable()
    .make()

  def mapsStream(n: Int): Stream[HTreeMap[Long, Digest]] = Stream.cons(
    db.hashMapCreate("map_" + n)
      .keySerializer(Serializer.LONG)
      .valueSerializer(Serializer.BYTE_ARRAY)
      .makeOrGet(),
    mapsStream(n + 1)
  )

  private val maps = mapsStream(0)


  override def set(key: Key, value: Digest): Unit = {
    maps(key._1.asInstanceOf[Int]).put(key._2, value)
  }


  override def commit(): Unit = {
    db.commit()
  }

  override def close(): Unit = {
    commit()
    db.close()
  }

  override def get(key: Key): Option[Digest] = {
    Try {
      maps(key._1).get(key._2)
    } match {
      case Failure(e) =>
        log.debug("Enable to load key: " + key)
        None
      case Success(v) =>
        Option(v)
    }
  }

}
