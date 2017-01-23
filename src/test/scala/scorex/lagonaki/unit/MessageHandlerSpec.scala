package scorex.lagonaki.unit

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.network.message.{BasicMessagesRepo, Message, MessageHandler}

class MessageHandlerSpec extends FunSuite with scorex.waves.TestingCommons {

  implicit val consensusModule = application.consensusModule
  implicit val transactionModule = application.transactionModule

  private lazy val repo = new BasicMessagesRepo()
  private lazy val handler = new MessageHandler(repo.specs)

  import Message._

  test("parseBytes have to verify Length field in parsing packet") {
    val s1 = BigInt(Long.MaxValue) * 1000000000L

    val msg = Message(repo.ScoreMessageSpec, Right(s1), None).bytes
    // patch length field from 12 bytes to 13
    msg.update(MagicLength + MessageCodeLength + 3, 13)

    val result = handler.parseBytes(ByteBuffer.wrap(msg))
    assert(result.isFailure)
  }

  test("parseBytes work for GetPeers message") {
    val msg = Message(repo.GetPeersSpec, Right(()), None).bytes
    val result = handler.parseBytes(ByteBuffer.wrap(msg))
    assert(result.isSuccess)
  }
}
