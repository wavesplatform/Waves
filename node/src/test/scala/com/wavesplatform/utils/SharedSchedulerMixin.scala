package com.wavesplatform.utils

import monix.execution.schedulers.SchedulerService
import org.scalatest.{BeforeAndAfterAll, Suite}

trait SharedSchedulerMixin extends BeforeAndAfterAll { _: Suite =>
  lazy val sharedScheduler: SchedulerService = Schedulers.fixedPool(1, schedulerName)

  protected def schedulerName: String = "heavy-request-processor"

  override def afterAll(): Unit = {
    sharedScheduler.shutdown()
    super.afterAll()
  }
}
