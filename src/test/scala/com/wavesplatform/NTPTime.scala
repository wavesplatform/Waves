package com.wavesplatform
import com.wavesplatform.utils.TimeImpl
import org.scalatest.{BeforeAndAfterAll, Suite}

trait NTPTime extends BeforeAndAfterAll { _: Suite =>
  protected val ntpTime = new TimeImpl

  override protected def afterAll(): Unit = {
    super.afterAll()
    ntpTime.close()
  }
}
