package com.wavesplatform.it.sync.grpc

import com.wavesplatform.it.{GrpcIntegrationSuiteWithThreeAddress, GrpcWaitForHeight, Nodes}
import org.scalatest._

trait GrpcBaseTransactionSuiteLike extends GrpcWaitForHeight with GrpcIntegrationSuiteWithThreeAddress { this: TestSuite with Nodes => }

abstract class GrpcBaseTransactionSuite extends funsuite.AnyFunSuite with GrpcBaseTransactionSuiteLike
