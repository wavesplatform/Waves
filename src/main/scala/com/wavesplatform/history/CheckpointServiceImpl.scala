package com.wavesplatform.history

import com.wavesplatform.db.{CheckpointCodec, PropertiesStorage, SubStorage}
import com.wavesplatform.network.Checkpoint
import com.wavesplatform.settings.CheckpointsSettings
import org.iq80.leveldb.DB
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{CheckpointService, ValidationError}

class CheckpointServiceImpl(db: DB, settings: CheckpointsSettings)
  extends SubStorage(db, "checkpoints") with PropertiesStorage with CheckpointService {

  private val CheckpointProperty = "checkpoint"

  override def get: Option[Checkpoint] = getProperty(CheckpointProperty).flatMap(b => CheckpointCodec.decode(b).toOption.map(r => r.value))

  override def set(cp: Checkpoint): Either[ValidationError, Unit] = for {
    _ <- Either.cond(!get.forall(_.signature sameElements cp.signature), (), GenericError("Checkpoint already applied"))
    _ <- Either.cond(EllipticCurveImpl.verify(cp.signature, cp.toSign, settings.publicKey.arr),
      putProperty(CheckpointProperty, CheckpointCodec.encode(cp)),
      GenericError("Invalid checkpoint signature"))
  } yield ()

}