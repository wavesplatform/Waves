package scorex.lagonaki.unit

import java.nio.ByteBuffer

import org.scalatest.FunSuite
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusModule
import scorex.crypto.EllipticCurveImpl.SignatureLength
import scorex.lagonaki.TestingCommons
import scorex.network.message.{BasicMessagesRepo, Message, MessageHandler}
import scorex.transaction.{History, SimpleTransactionModule}
import shapeless.syntax.typeable._

class MessageSpecification extends FunSuite with TestingCommons {

  private lazy val repo = new BasicMessagesRepo()
  private lazy val handler = new MessageHandler(repo.specs)

  test("ScoreMessage roundtrip 1") {
    val s1 = BigInt(Long.MaxValue) * 1000000000L

    val msg = Message(repo.ScoreMessageSpec, Right(s1), None)

    handler.parse(ByteBuffer.wrap(msg.bytes), None).get.data.get match {
      case scoreRestored: History.BlockchainScore =>
        assert(s1 == scoreRestored)

      case _ =>
        fail("wrong data type restored")
    }
  }

  test("GetSignaturesMessage roundtrip 1") {
    val e1 = 33: Byte
    val e2 = 34: Byte
    val s1: Block.BlockId = e2 +: Array.fill(SignatureLength - 1)(e1)

    val msg = Message(repo.GetSignaturesSpec, Right(Seq(s1)), None)
    val ss = handler.parse(ByteBuffer.wrap(msg.bytes), None).get.data.get.asInstanceOf[Seq[Block.BlockId]]
    assert(ss.head.sameElements(s1))
  }

  test("SignaturesMessage roundtrip 1") {
    val e1 = 33: Byte
    val e2 = 34: Byte
    val s1 = e2 +: Array.fill(SignatureLength - 1)(e1)
    val s2 = e1 +: Array.fill(SignatureLength - 1)(e2)

    val msg = Message(repo.SignaturesSpec, Right(Seq(s1, s2)), None)
    val ss = handler.parse(ByteBuffer.wrap(msg.bytes), None).get.data.get.asInstanceOf[Seq[Block.BlockId]]
    assert(ss.head.sameElements(s1))
    assert(ss.tail.head.sameElements(s2))
  }
}