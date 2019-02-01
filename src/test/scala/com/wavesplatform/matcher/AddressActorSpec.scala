package com.wavesplatform.matcher
import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{FreeSpecLike, Matchers}

class AddressActorSpec extends TestKit(ActorSystem()) with FreeSpecLike with Matchers {
  "schedule order cancellation" in {
    pending
  }
}
