package com.wavesplatform.test

import com.wavesplatform.utils.ApplicationStopReason

import java.security.Permission
import java.util.concurrent.Semaphore
import scala.annotation.nowarn

trait HasSecurityManager {

  /** @return
    *   Stop reason code
    */
  @nowarn("cat=deprecation")
  protected def withSecurityManager(okIf: ApplicationStopReason)(f: Semaphore => Unit): Int = {
    var stopReasonCode = 0

    val signal = new Semaphore(1)
    signal.acquire()

    System.setSecurityManager(new SecurityManager {
      override def checkPermission(perm: Permission): Unit = {}

      override def checkPermission(perm: Permission, context: Object): Unit = {}

      override def checkExit(status: Int): Unit = signal.synchronized {
        super.checkExit(status)
        stopReasonCode = status
        if (status == okIf.code)
          signal.release()
        throw new SecurityException("System exit is not allowed")
      }
    })

    try {
      f(signal)
      stopReasonCode
    } finally System.setSecurityManager(null)
  }
}
