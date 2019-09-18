package com.wavesplatform.api.grpc
import com.google.protobuf.empty.Empty
import com.wavesplatform.events.protobuf.{BlockchainUpdated, PBEvents, VanillaBlockchainUpdated}
import io.grpc.stub.StreamObserver
import monix.execution.Scheduler
import monix.reactive.Observable

class EventsApiGrpcImpl(stream: Observable[VanillaBlockchainUpdated])(implicit sc: Scheduler) extends EventsApiGrpc.EventsApi {
  override def getEventStream(request: Empty, responseObserver: StreamObserver[BlockchainUpdated]): Unit = {
    val resultStream = stream.observeOn(sc).map(PBEvents.protobuf)
    responseObserver.completeWith(resultStream)
  }
}
