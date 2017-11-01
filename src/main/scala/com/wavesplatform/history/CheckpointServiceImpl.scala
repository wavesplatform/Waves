package com.wavesplatform.history

import java.io.File
import java.io.File

import com.google.common.primitives._
import com.twitter.chill.{KryoInstantiator, KryoPool}
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.CheckpointsSettings
import com.wavesplatform.utils.createStore
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{CheckpointService, ValidationError}

class CheckpointServiceImpl(file: File, settings: CheckpointsSettings) extends CheckpointService with AutoCloseable {

  import CheckpointServiceImpl._

  private val db = createStore(file)
  private val key = 0

  override def get: Option[Checkpoint] = {
    val keyBytes = Ints.toByteArray(key)
    Option(decode(db.get(keyBytes))).map { case (seq, sig) => Checkpoint(seq.map(BlockCheckpoint.tupled), sig) }
  }

  override def set(cp: Checkpoint): Either[ValidationError, Unit] = for {
    _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
    _ <- Either.cond(EllipticCurveImpl.verify(cp.signature, cp.toSign, settings.publicKey.arr),
      db.put(Ints.toByteArray(key), encode((cp.items.map(bcp => (bcp.height, bcp.signature)), cp.signature))),
      GenericError("Invalid checkpoint signature"))
  } yield ()

  override def close(): Unit = db.close()
}

object CheckpointServiceImpl {
  private val POOL_SIZE = 10
  private val kryo = KryoPool.withByteArrayOutputStream(POOL_SIZE, new KryoInstantiator())

  def encode(value: (Seq[(Int, Array[Byte])], Array[Byte])): Array[Byte] = kryo.toBytesWithClass(value)

  def decode(arr: Array[Byte]): (Seq[(Int, Array[Byte])], Array[Byte]) = kryo.fromBytes(arr, classOf[(Seq[(Int, Array[Byte])], Array[Byte])])
}