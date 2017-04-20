package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.network.Checkpoint
import scorex.transaction.CheckpointService
import scorex.utils.LogMVMapBuilder

trait CheckpointStorage {
  val checkpoint: java.util.Map[Int, Checkpoint]

  def commit(): Unit
}

class MVStoreCheckpointStorage(db: MVStore) extends CheckpointStorage {
  val checkpoint: MVMap[Int, Checkpoint] = db.openMap("checkpoint", new LogMVMapBuilder[Int, Checkpoint])

  def commit(): Unit = db.commit()
}

class CheckpointServiceImpl(storage: CheckpointStorage) extends CheckpointService {

  override def getCheckpoint: Option[Checkpoint] = Option(storage.checkpoint.get(0))

  override def setCheckpoint(c: Option[Checkpoint]): Unit = {
    if (c.isDefined) storage.checkpoint.put(0, c.get)
    else storage.checkpoint.remove(0)

    storage.commit()
  }
}