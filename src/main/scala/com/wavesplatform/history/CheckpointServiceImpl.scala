package com.wavesplatform.history

import com.twitter.chill.{KryoInstantiator, KryoPool}
import com.wavesplatform.db.{PropertiesStorage, SubStorage}
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.CheckpointsSettings
import org.iq80.leveldb.DB
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{CheckpointService, ValidationError}

class CheckpointServiceImpl(db: DB, settings: CheckpointsSettings)
  extends SubStorage(db, "checkpoints") with PropertiesStorage with CheckpointService {

  import CheckpointServiceImpl._

  private val CheckpointProperty = "checkpoint"

  override def get: Option[Checkpoint] = get(CheckpointProperty).map(decode).map { case (seq, sig) =>
    Checkpoint(seq.map(BlockCheckpoint.tupled), sig)
  }

  override def set(cp: Checkpoint): Either[ValidationError, Unit] = for {
    _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
    _ <- Either.cond(EllipticCurveImpl.verify(cp.signature, cp.toSign, settings.publicKey.arr),
      put(CheckpointProperty, encode((cp.items.map(bcp => (bcp.height, bcp.signature)), cp.signature))),
      GenericError("Invalid checkpoint signature"))
  } yield ()

}

object CheckpointServiceImpl {
  private val POOL_SIZE = 10
  private val kryo = KryoPool.withByteArrayOutputStream(POOL_SIZE, new KryoInstantiator())

  def encode(value: (Seq[(Int, Array[Byte])], Array[Byte])): Array[Byte] = kryo.toBytesWithClass(value)

  def decode(arr: Array[Byte]): (Seq[(Int, Array[Byte])], Array[Byte]) = kryo.fromBytes(arr, classOf[(Seq[(Int, Array[Byte])], Array[Byte])])
}