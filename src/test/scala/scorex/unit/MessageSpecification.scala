package scorex.unit

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.network.message.{ScoreMessage, Message, PingMessage}

import scala.util.Random

class MessageSpecification extends FunSuite {
  test("ping message roundtrip") {
    val rnd = Random.nextInt()

    val msg = PingMessage
    val parsedTry = Message.parse(ByteBuffer.wrap(msg.toBytes()))

    assert(parsedTry.isSuccess)
  }

  test("score message roundtrip 1") {
    val h1 = 1
    val s1 = BigInt(2)

    val msg = ScoreMessage(h1, s1)
    val parsed = Message.parse(ByteBuffer.wrap(msg.toBytes())).get

    assert(parsed.isInstanceOf[ScoreMessage])
    assert(parsed.asInstanceOf[ScoreMessage].height == h1)
    assert(parsed.asInstanceOf[ScoreMessage].score == s1)
  }

  test("score message roundtrip 2") {
    val h1 = Int.MaxValue-1
    val s1 = BigInt(Long.MaxValue)+100

    val msg = ScoreMessage(h1, s1)
    val parsed = Message.parse(ByteBuffer.wrap(msg.toBytes())).get

    assert(parsed.isInstanceOf[ScoreMessage])
    assert(parsed.asInstanceOf[ScoreMessage].height == h1)
    assert(parsed.asInstanceOf[ScoreMessage].score == s1)
  }
}

