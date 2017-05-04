package scorex.lagonaki.unit

import java.nio.ByteBuffer

import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSuite
import scorex.network.message._
import scorex.transaction.NewTransactionHandler

class MessageHandlerSpec extends FunSuite with MockFactory with UnitTestConfig {

  implicit val transactionModule = mock[NewTransactionHandler]

  private lazy val handler = new MessageHandler(BasicMessagesRepo.specs)

  import Message._

  test("parseBytes have to verify Length field in parsing packet") {
    val s1 = BigInt(Long.MaxValue) * 1000000000L

    val msg = Message(ScoreMessageSpec, Right(s1), None).bytes
    // patch length field from 12 bytes to 13
    msg.update(MagicLength + MessageCodeLength + 3, 13)

    val result = handler.parseBytes(ByteBuffer.wrap(msg))
    assert(result.isFailure)
  }

  test("parseBytes work for GetPeers message") {
    val msg = Message(GetPeersSpec, Right(()), None).bytes
    val result = handler.parseBytes(ByteBuffer.wrap(msg))
    assert(result.isSuccess)
  }
}
