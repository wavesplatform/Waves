package scorex.perma.storage

import org.mapdb.{HTreeMap, Serializer}
import scorex.crypto.ads.merkle.AuthDataBlock
import scorex.perma.settings.PermaConstants.{DataSegment, DataSegmentIndex}
import scorex.storage.MapDBStorage

class AuthDataStorage(fileName: String) extends MapDBStorage[DataSegmentIndex, AuthDataBlock[DataSegment]](fileName) {

  override protected val map: HTreeMap[DataSegmentIndex, AuthDataBlock[DataSegment]] = db.hashMapCreate("segments")
    .keySerializer(Serializer.LONG)
    .makeOrGet()

}
