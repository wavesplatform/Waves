package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.network.{BlockCheckpoint, Checkpoint}
import scorex.transaction.CheckpointService
import scorex.utils.LogMVMapBuilder

trait CheckpointStorage {
  val checkpoint: java.util.Map[Int, (Seq[(Int, Array[Byte])], Array[Byte])]

  def commit(): Unit
}

class MVStoreCheckpointStorage(db: MVStore) extends CheckpointStorage {
  val checkpoint: MVMap[Int, (Seq[(Int, Array[Byte])], Array[Byte])] = db.openMap("checkpoint", new LogMVMapBuilder[Int, (Seq[(Int, Array[Byte])], Array[Byte])])

  def commit(): Unit = db.commit()
}

class CheckpointServiceImpl(storage: CheckpointStorage) extends CheckpointService {

  private val key = 0

  override def getCheckpoint: Option[Checkpoint] =
    Option(storage.checkpoint.get(key))
      .map { case (seq, sig) => Checkpoint(seq.map(BlockCheckpoint.tupled), sig) }

  override def setCheckpoint(c: Option[Checkpoint]): Unit = {
    c match {
      case Some(cp) => storage.checkpoint.put(key, (cp.items.map(bcp => (bcp.height, bcp.signature)), cp.signature))
      case None => storage.checkpoint.remove(key)
    }
    storage.commit()
  }
}