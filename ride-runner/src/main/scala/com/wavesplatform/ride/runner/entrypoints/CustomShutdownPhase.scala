package com.wavesplatform.ride.runner.entrypoints

sealed abstract class CustomShutdownPhase(private val nameSuffix: String) extends Product with Serializable {
  val name = s"ride-$nameSuffix"
}

object CustomShutdownPhase {
  case object Metrics                 extends CustomShutdownPhase("metrics")
  case object ThreadPools             extends CustomShutdownPhase("thread-pools")
  case object GrpcConnector           extends CustomShutdownPhase("grpc-connector")
  case object ApiClient               extends CustomShutdownPhase("api-client")
  case object Db                      extends CustomShutdownPhase("db")
  case object BlockchainUpdatesStream extends CustomShutdownPhase("blockchain-updates-stream")
}
