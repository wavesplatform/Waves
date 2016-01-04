package scorex.perma.storage

import java.io.File

import org.mapdb.{DBMaker, HTreeMap, Serializer}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.settings.PermaConstants.{DataSegment, DataSegmentIndex}
import scorex.storage.Storage
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success, Try}

class AuthDataStorage(fileName: String) extends Storage[DataSegmentIndex, AuthDataBlock[DataSegment]] with ScorexLogging {

  private val db =
    DBMaker.appendFileDB(new File(fileName))
      .fileMmapEnableIfSupported()
      .closeOnJvmShutdown()
      .checksumEnable()
      .make()

  private val map: HTreeMap[DataSegmentIndex, AuthDataBlock[DataSegment]] =
    db.hashMapCreate("segments")
      .keySerializer(Serializer.LONG)
      .makeOrGet()

  override def set(key: DataSegmentIndex, value: AuthDataBlock[DataSegment]): Unit =
    Try(map.put(key, value)).recoverWith { case t: Throwable =>
      log.warn("Failed to set key:" + key, t)
      Failure(t)
    }

  override def commit(): Unit = db.commit()

  override def close(): Unit = db.close()

  override def containsKey(key: DataSegmentIndex): Boolean = map.containsKey(key)

  override def get(key: DataSegmentIndex): Option[AuthDataBlock[DataSegment]] =
    Try(map.get(key)) match {
      case Success(v) =>
        Option(v)

      case Failure(e) =>
        log.debug("Enable to load key for level 0: " + key)
        None
    }
}
