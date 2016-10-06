package scorex.utils

import scala.util.control.NonFatal
import org.h2.mvstore.MVMap
import org.h2.mvstore.`type`.ObjectDataType

class LogMVMapBuilder[K, V] extends MVMap.Builder[K, V] with ScorexLogging {
  override def create(): MVMap[K, V] = {
    if (keyType == null) keyType = new ObjectDataType
    if (valueType == null) valueType = new ObjectDataType
    new MVMap[K, V](keyType, valueType) {
      override def put(key: K, value: V): V = {
        try super.put(key, value) catch {
          case NonFatal(t) =>
            log.error("MVStore put error", t)
            throw t
        }
      }
      override def get(key: scala.Any): V = try super.get(key) catch {
        case NonFatal(t) =>
          log.error("MVStore get error", t)
          throw t
      }
    }
  }
}
