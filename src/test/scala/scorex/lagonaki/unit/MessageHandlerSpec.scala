package scorex.lagonaki.unit

import java.nio.ByteBuffer

import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSuite
import scorex.consensus.nxt.WavesConsensusModule
import scorex.network.message.{BasicMessagesRepo, Message, MessageHandler}
import scorex.transaction.{SimpleTransactionModule, TransactionModule}

class MessageHandlerSpec extends FunSuite with MockFactory with UnitTestConfig {

  implicit val consensusModule = new WavesConsensusModule(blockchainSettings)
  implicit val transactionModule = mock[TransactionModule]

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
