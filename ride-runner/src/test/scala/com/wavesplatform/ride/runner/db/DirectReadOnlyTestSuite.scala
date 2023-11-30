package com.wavesplatform.ride.runner.db

class DirectReadOnlyTestSuite extends ReadOnlyTestSuite {
  override protected def readOnly[T](dbAccess: RideDbAccess)(f: ReadOnly => T): T   = dbAccess.directReadWrite(f)
  override protected def readWrite[T](dbAccess: RideDbAccess)(f: ReadWrite => T): T = dbAccess.directReadWrite(f)
}
