//package scorex.lagonaki.unit
//
//import java.nio.ByteBuffer
//
//import com.wavesplatform.state2.EqByteArray
//import org.scalamock.scalatest.MockFactory
//import org.scalatest.FunSuite
//import scorex.block.Block
//import scorex.network.message._
//import scorex.transaction.TransactionParser._
//import scorex.transaction.{History, NewTransactionHandler}
//
//import scala.util.Try
//
//class MessageSpecification extends FunSuite with MockFactory {
//
//  private lazy val handler = new MessageHandler(BasicMessagesRepo.specs)
//
//  private def toMessage(v: Try[(MessageSpec[_], Array[Byte])]): Try[Message[Any]] =
//    v.map { case (spec, bytes) => Message(spec, Left(bytes), None) }
//
//  test("ScoreMessage roundtrip 1") {
//    val s1 = BigInt(Long.MaxValue) * 1000000000L
//
//    val msg = Message(ScoreMessageSpec, Right(s1), None)
//
//    toMessage(handler.parseBytes(ByteBuffer.wrap(msg.bytes))).get.data.get match {
//      case scoreRestored: History.BlockchainScore =>
//        assert(s1 == scoreRestored)
//
//      case _ =>
//        fail("wrong data type restored")
//    }
//  }
//
//  test("GetSignaturesMessage roundtrip 1") {
//    val e1 = 33: Byte
//    val e2 = 34: Byte
//    val s1: Block.BlockId = EqByteArray(e2 +: Array.fill(SignatureLength - 1)(e1))
//
//    val msg = Message(GetSignaturesSpec, Right(Seq(s1).map(_.arr)), None)
//    val ss = toMessage(handler.parseBytes(ByteBuffer.wrap(msg.bytes))).get.data.get.asInstanceOf[Seq[Block.BlockId]]
//    assert(ss.head.==(s1))
//  }
//
//  test("SignaturesMessage roundtrip 1") {
//    val e1 = 33: Byte
//    val e2 = 34: Byte
//    val s1 = EqByteArray(e2 +: Array.fill(SignatureLength - 1)(e1))
//    val s2 = EqByteArray(e1 +: Array.fill(SignatureLength - 1)(e2))
//
//    val msg = Message(SignaturesSpec, Right(Seq(s1, s2).map(_.arr)), None)
//    val ss = toMessage(handler.parseBytes(ByteBuffer.wrap(msg.bytes))).get.data.get.asInstanceOf[Seq[Block.BlockId]]
//    assert(ss.head.==(s1))
//    assert(ss.tail.head.==(s2))
//  }
//}
