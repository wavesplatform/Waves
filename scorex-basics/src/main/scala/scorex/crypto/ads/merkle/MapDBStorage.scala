package scorex.crypto.ads.merkle

import java.io.File
import java.util.concurrent.ConcurrentNavigableMap

import org.mapdb.DBMaker
import scorex.crypto.CryptographicHash.Digest

import scala.util.Try

class MapDBStorage(file: File) extends Storage {

  import Storage._

  val db = DBMaker.fileDB(file)
    .fileMmapEnableIfSupported()
    .closeOnJvmShutdown()
    .checksumEnable()
    .make()

  val map: ConcurrentNavigableMap[String, Digest] = db.treeMap("tree")

  override def set(key: Key, value: Digest): Try[Digest] = {
    Try {
      map.put(stringKey(key), value)
    }
  }


  override def commit(): Unit = {
    db.commit()
    db.close()
  }

  override def close(): Unit = {
    db.close()
  }

  override def get(key: Key): Option[Digest] = {
    Option(map.get(stringKey(key)))
  }

  private def stringKey(key: Key): String = {
    key._1 + "_" + key._2
  }
}
