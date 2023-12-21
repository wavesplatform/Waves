package com.wavesplatform.utils

import java.util
import java.util.concurrent.{AbstractExecutorService, TimeUnit}

object RunNowExecutorService extends AbstractExecutorService {
  override def shutdown(): Unit                                         = {}
  override def shutdownNow(): util.List[Runnable]                       = util.Collections.emptyList()
  override def isShutdown: Boolean                                      = false
  override def isTerminated: Boolean                                    = false
  override def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = true
  override def execute(command: Runnable): Unit                         = command.run()
}
