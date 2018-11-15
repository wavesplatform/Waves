package com.wavesplatform.matcher

import java.util.concurrent.ConcurrentHashMap

import akka.stream.scaladsl.Source
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.model.Command

import scala.concurrent.{Future, Promise}

trait Bridge {
  // def commands: Source[Command, ]
  def send(command: Command): Future[MatcherResponse]
}

object Bridge {

  class Local extends Bridge {
    private val requests = new ConcurrentHashMap[Command.Id, Promise[MatcherResponse]]()

    def ready(id: Command.Id, response: MatcherResponse): Unit = Option(requests.remove(id)).foreach(_.trySuccess(response))

    override def send(command: Command): Future[MatcherResponse] = {
      val id = Command.newId
      val r  = Promise[MatcherResponse]()
      requests.put(id, r)
      r.future
    }
  }

}
