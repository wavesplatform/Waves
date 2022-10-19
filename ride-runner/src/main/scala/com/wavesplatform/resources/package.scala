package com.wavesplatform

import io.grpc.{ManagedChannel, Server}

import java.util.concurrent.{ExecutorService, TimeUnit}
import scala.util.Using.Releasable

package object resources {
  implicit def releasableExecutorService[T <: ExecutorService]: Releasable[T] = _.shutdown()

  implicit final val releasableServer: Releasable[Server] = { resource =>
    resource.shutdown()
    try resource.awaitTermination(5, TimeUnit.SECONDS)
    finally resource.shutdownNow()
  }

  implicit final val releasableChannel: Releasable[ManagedChannel] = { resource =>
    resource.shutdown()
    try resource.awaitTermination(5, TimeUnit.SECONDS)
    finally resource.shutdownNow()
  }
}
