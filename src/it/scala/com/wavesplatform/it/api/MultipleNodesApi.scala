package com.wavesplatform.it.api

import java.util.{Timer, TimerTask}

import com.wavesplatform.it.api.MultipleNodesApi._
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Future, Promise}

trait MultipleNodesApi extends ScorexLogging {
  def waitFor[A](desc: String)(nodes: Iterable[NodeApi], retryInterval: FiniteDuration)
                (request: NodeApi => Future[A], cond: Iterable[A] => Boolean): Future[Boolean] = {
    def retry = sleep(retryInterval).flatMap { _ =>
      waitFor(desc)(nodes, retryInterval)(request, cond)
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
    def waitHeight = waitFor[Int](s"all heights >= $height")(nodes, retryInterval)(_.height, _.forall(_ >= height))

    def waitSameBlocks = waitFor[NodeApi.Block](s"same blocks at height = $height")(nodes, retryInterval)(_.blockAt(height), { blocks =>
      val sig = blocks.map(_.signature)
      sig.forall(_ == sig.head)
    })

    for {
      _ <- waitHeight
      _ = log.debug(s"All nodes reached height $height")
      r <- waitSameBlocks
    } yield r
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
