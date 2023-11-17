package com.wavesplatform.api

import io.grpc.{MethodDescriptor, Status}
import kamon.Kamon
import kamon.metric.{Counter, Timer}
import kamon.tag.TagSet

object GrpcStats {
  private val statusCodeStats = {
    val commonGrpcStatus = Kamon.counter("grpc.status")
    Status.Code.values().map { code => code.value() -> commonGrpcStatus.withTag("code", code.name()) }.toMap
  }
  def status(method: GrpcMethod, code: Status.Code): Counter = statusCodeStats(code.value()).withTags(method.tagSet)

  private val latencyStat                = Kamon.timer("grpc.latency")
  def latency(method: GrpcMethod): Timer = latencyStat.withTags(method.tagSet)

  private val blockedCallStat = Kamon
    .counter(
      "grpc.blocked",
      "A number of blocked calls. If this number is high, you could want to relax a limit of concurrent connections on a frontend of gRPC server"
    )
    .withoutTags()
  def blockedCall(method: GrpcMethod): Counter = blockedCallStat.withTags(method.tagSet)
}

object GrpcMethod {
  def of(method: MethodDescriptor[?, ?]): GrpcMethod = {
    val serviceName = MethodDescriptor.extractFullServiceName(method.getFullMethodName)
    // Full method names are of the form: "full.serviceName/MethodName". We extract the last part.
    val methodName = method.getFullMethodName.substring(serviceName.length + 1)
    GrpcMethod(serviceName, methodName)
  }
}

case class GrpcMethod(serviceName: String, methodName: String) {
  def tagSet: TagSet = TagSet.Empty.withTag("service", serviceName).withTag("method", methodName)
}
