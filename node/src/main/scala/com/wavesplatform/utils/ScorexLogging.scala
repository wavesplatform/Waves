package com.wavesplatform.utils

import com.typesafe.scalalogging.Logger
import monix.eval.Task
import monix.execution.{CancelableFuture, Scheduler}
import monix.reactive.Observable
import org.slf4j.LoggerFactory

trait ScorexLogging {
  protected lazy val log = Logger(LoggerFactory.getLogger(this.getClass))

  implicit class TaskExt[A](t: Task[A]) {
    def runAsyncLogErr(implicit s: Scheduler): CancelableFuture[A] =
      logErr.runToFuture

    def logErr: Task[A] = {
      t.onErrorHandleWith(ex => {
        log.error(s"Error executing task", ex)
        Task.raiseError[A](ex)
      })
    }
  }

  implicit class ObservableExt[A](o: Observable[A]) {

    def logErr: Observable[A] = {
      o.onErrorHandleWith(ex => {
        log.error(s"Error observing item", ex)
        Observable.raiseError[A](ex)
      })
    }
  }
}
