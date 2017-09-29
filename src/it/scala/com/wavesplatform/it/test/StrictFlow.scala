package com.wavesplatform.it.test

import org.scalatest._

trait StrictFlow extends TestSuite {

  private var isSuiteFailed = false

  override abstract protected def withFixture(test: NoArgTest): Outcome = {
    if (isSuiteFailed) Canceled("A suite with the strict flow is failed, no reason to continue")
    else {
      val r = super.withFixture(test)
      r match {
        case Failed(_) => isSuiteFailed = true
        case _ =>
      }
      r
    }
  }

}
