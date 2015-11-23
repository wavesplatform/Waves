package scorex.perma.Storage

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import org.slf4j.LoggerFactory
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.Parameters.DataSegment
import scorex.storage.Storage

import scala.util.{Failure, Success, Try}

class AuthDataStorage(fileName: String) extends Storage[Long, AuthDataBlock[DataSegment]] {

  private val log = LoggerFactory.getLogger(this.getClass)

  private val db =
    DBMaker.appendFileDB(new File(fileName + ".mapDB"))
      .fileMmapEnableIfSupported()
      .closeOnJvmShutdown()
      .checksumEnable()
      .make()

  private val map: HTreeMap[Long, AuthDataBlock[DataSegment]] = db.hashMapCreate("segments")
    .keySerializer(Serializer.LONG)
    .makeOrGet()

  override def set(key: Long, value: AuthDataBlock[DataSegment]): Unit = {
    Try {
      map.put(key, value)
    }.recoverWith { case t: Throwable =>
      log.warn("Failed to set key:" + key, t)
      Failure(t)
    }
  }

  override def commit(): Unit = db.commit()

  override def close(): Unit = {
    commit()
    db.close()
  }

  override def containsKey(key: Long): Boolean = {
    map.containsKey(key)
  }

  override def get(key: Long): Option[AuthDataBlock[DataSegment]] = {
    Try {
      map.get(key)
    } match {
      case Success(v) =>
        Option(v)

      case Failure(e) =>
        log.debug("Enable to load key for level 0: " + key)
        None
    }
  }

}
