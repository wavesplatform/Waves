package com.wavesplatform.history

import java.io.File

import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.CheckpointsSettings
import com.wavesplatform.utils.createMVStore
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{CheckpointService, ValidationError}
import scorex.utils.LogMVMapBuilder

class CheckpointServiceImpl(fileName: Option[File], settings: CheckpointsSettings) extends CheckpointService with AutoCloseable {
  private val db = createMVStore(fileName)
  private val checkpoint = db.openMap("checkpoint", new LogMVMapBuilder[Int, (Seq[(Int, Array[Byte])], Array[Byte])])
  private val key = 0

  override def get: Option[Checkpoint] =
    Option(checkpoint.get(key))
      .map { case (seq, sig) => Checkpoint(seq.map(BlockCheckpoint.tupled), sig) }

  override def set(cp: Checkpoint): Either[ValidationError, Unit] = for {
    _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
    _ <- Either.cond(EllipticCurveImpl.verify(cp.signature, cp.toSign, settings.publicKey.arr),
      checkpoint.put(key, (cp.items.map(bcp => (bcp.height, bcp.signature)), cp.signature)),
      GenericError("Invalid checkpoint signature"))
  } yield db.commit()


  override def close(): Unit = db.close()
}