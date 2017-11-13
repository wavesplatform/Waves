package com.wavesplatform.it.api

import java.util.{Timer, TimerTask}

import com.wavesplatform.it.api.MultipleNodesApi._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

trait MultipleNodesApi {
  def waitFor[A](nodes: Iterable[NodeApi], retryInterval: FiniteDuration)
                (request: NodeApi => Future[A], cond: Iterable[A] => Boolean): Future[Boolean] = {
    def retry = sleep(retryInterval).flatMap { _ =>
      waitFor(nodes, retryInterval)(request, cond)
    }

    Future.traverse(nodes)(request)
      .map(cond)
      .recover { case _ => false }
      .flatMap {
        case true => Future.successful(true)
        case false => retry
      }
  }

  def waitForSameBlocksAt(nodes: Iterable[NodeApi], retryInterval: FiniteDuration, height: Int): Future[Boolean] = {
    waitFor[NodeApi.Block](nodes, retryInterval)(_.blockAt(height), { blocks =>
      val sig = blocks.map(_.signature)
      sig.forall(_ == sig.head)
    })
  }
}

object MultipleNodesApi {
  private val timer = new Timer("multiple-nodes-api", true)

  private def sleep(delay: FiniteDuration): Future[Unit] = {
    val p = Promise[Unit]()
    timer.schedule(
      new TimerTask {
        override def run(): Unit = p.success(())
      },
      delay.toMillis)
    p.future
  }
}
