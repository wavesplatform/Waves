package com.wavesplatform.history

import java.io.File

import com.wavesplatform.utils.createMVStore
import scorex.network.{BlockCheckpoint, Checkpoint}
import scorex.transaction.CheckpointService
import scorex.utils.LogMVMapBuilder

class CheckpointServiceImpl(fileName: Option[File]) extends CheckpointService with AutoCloseable {
  private val db = createMVStore(fileName)
  private val checkpoint = db.openMap("checkpoint", new LogMVMapBuilder[Int, (Seq[(Int, Array[Byte])], Array[Byte])])
  private val key = 0

  override def get: Option[Checkpoint] =
    Option(checkpoint.get(key))
      .map { case (seq, sig) => Checkpoint(seq.map(BlockCheckpoint.tupled), sig) }

  override def set(c: Option[Checkpoint]): Unit = {
    c match {
      case Some(cp) => checkpoint.put(key, (cp.items.map(bcp => (bcp.height, bcp.signature)), cp.signature))
      case None => checkpoint.remove(key)
    }
    db.commit()
  }

  override def close() = db.close()
}