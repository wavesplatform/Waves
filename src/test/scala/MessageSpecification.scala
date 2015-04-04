package scorex.test

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.network.message.{Message, PingMessage}

import scala.util.Random

class MessageSpecification extends FunSuite {
  test("ping message roundtrip"){
    val rnd = Random.nextInt()

    val msg = PingMessage(Some(rnd))
    val parsedTry = Message.parse(ByteBuffer.wrap(msg.toBytes()))

    assert(parsedTry.isSuccess)
    assert(parsedTry.get.mbId.get == rnd)
  }
}

