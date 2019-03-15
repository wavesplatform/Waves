package com.wavesplatform.protobuf.transaction
import com.google.protobuf.{ByteString, CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.FastHashId
import monix.eval.Coeval

trait PBCachedTransaction {
  def transaction: SignedTransaction
  def bodyBytes: Array[Byte]
  def bytes: Array[Byte]
  val id: Coeval[ByteStr] = Coeval.evalOnce(FastHashId.create(this.bodyBytes))

  override def equals(obj: Any): Boolean = obj match {
    case ct: PBCachedTransaction => transaction.equals(ct.transaction)
    case _                       => transaction.equals(obj)
  }

  override def hashCode(): Int  = transaction.hashCode()
  override def toString: String = transaction.toString
}

object PBCachedTransaction {
  def fromBytes(bs: ByteStr): PBCachedTransaction = new PBCachedTransaction {
    private[this] val transactionCoeval = Coeval.evalOnce(PBSignedTransaction.parseFrom(bs))
    private[this] val bodyBytesCoeval = Coeval.evalOnce {
      val codedInputStream = CodedInputStream.newInstance(bs)
      var done             = false
      var bytes            = Option.empty[ByteString]
      while (!done) {
        val tag = codedInputStream.readTag()
        tag match {
          case 0 => done = true
          case 10 =>
            bytes = Some(codedInputStream.readBytes())
            done = true
          case tag => codedInputStream.skipField(tag)
        }
      }
      bytes.fold(transactionCoeval().getTransaction.toByteArray)(_.toByteArray)
    }

    override def transaction: PBSignedTransaction = transactionCoeval()
    override def bodyBytes: Array[Byte]           = bodyBytesCoeval()
    override def bytes: Array[Byte]               = bs
  }

  implicit def apply(tx: PBSignedTransaction): PBCachedTransaction = new PBCachedTransaction {
    private[this] val bodyBytesCoeval = Coeval.evalOnce(tx.getTransaction.toByteArray)
    private[this] val bytesCoeval = Coeval.evalOnce {
      val outArray  = new Array[Byte](tx.serializedSize)
      val _output__ = CodedOutputStream.newInstance(outArray)
      tx.transaction.foreach { __v =>
        val __m = __v
        _output__.writeTag(1, 2)
        _output__.writeUInt32NoTag(__m.serializedSize)
        // __m.writeTo(_output__)
        _output__.writeBytesNoTag(ByteString.copyFrom(bodyBytesCoeval()))
      }
      tx.proofs.foreach { __v =>
        val __m = __v
        _output__.writeBytes(2, __m)
      }
      _output__.flush()
      _output__.checkNoSpaceLeft()
      outArray
    }

    override def transaction: PBSignedTransaction = tx
    override def bodyBytes: ByteStr               = bodyBytesCoeval()
    override def bytes: ByteStr                   = bytesCoeval()
  }
}
