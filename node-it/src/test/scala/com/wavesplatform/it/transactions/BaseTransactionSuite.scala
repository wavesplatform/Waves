package com.wavesplatform.it.transactions

import com.wavesplatform.it._
import org.scalatest.{FunSuite, TestSuite}

trait BaseTransactionSuiteLike extends BaseSuiteLike with IntegrationSuiteWithThreeAddresses {
  this: TestSuite =>
}

abstract class BaseTransactionSuite extends FunSuite with BaseTransactionSuiteLike
