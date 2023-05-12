package com.wavesplatform.ride.runner.db

class BatchedReadOnlyTestSuite extends ReadOnlyTestSuite {
  override protected def readOnly[T](dbAccess: RideDbAccess)(f: ReadOnly => T): T   = dbAccess.batchedReadOnly(f)
  override protected def readWrite[T](dbAccess: RideDbAccess)(f: ReadWrite => T): T = dbAccess.batchedReadWrite(f)
}
