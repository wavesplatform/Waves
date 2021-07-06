package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest._

trait BaseTransactionSuiteLike extends WaitForHeight2 with IntegrationSuiteWithThreeAddresses with BeforeAndAfterAll with NodesFromDocker {
  this: TestSuite with Nodes =>

}

abstract class BaseTransactionSuite extends funsuite.AnyFunSuite with BaseTransactionSuiteLike
