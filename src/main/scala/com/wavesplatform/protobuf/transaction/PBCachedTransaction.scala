package com.wavesplatform.protobuf.transaction
import com.google.protobuf.{ByteString, CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{FastHashId, Proofs}
import monix.eval.Coeval
import scalapb.GeneratedMessage

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
  implicit def apply(tx: PBTransaction): PBCachedTransaction = new PBCachedTransactionImplWithBodyBytes(tx, Nil)
  implicit def apply(tx: PBSignedTransaction): PBCachedTransaction = new PBCachedTransactionImplWithBytes(tx)
  def fromBytes(bytes: Array[Byte]): PBCachedTransaction = new PBCachedTransactionImplWithBytes(bytes)
  def fromBodyBytes(bodyBytes: Array[Byte]): PBCachedTransaction = new PBCachedTransactionImplWithBodyBytes(bodyBytes, Nil)

  implicit class PBCachedTransactionImplicitOps(private val tx: PBCachedTransaction) extends AnyVal {
    def withProofs(proofs: ByteString*): PBCachedTransaction = tx match {
      case bbtx: PBCachedTransactionImplWithBodyBytes =>
        new PBCachedTransactionImplWithBodyBytes(bbtx.underlying, proofs)

      case _ =>
        new PBCachedTransactionImplWithBodyBytes(tx.bodyBytes, proofs)
    }

    def withProofs(proof1: ByteStr, proofs: ByteStr*): PBCachedTransaction = withProofs((proof1 +: proofs).map(ByteString.copyFrom(_)): _*)

    def withProofs(proofs: Proofs): PBCachedTransaction = withProofs(proofs.proofs.map(ByteString.copyFrom(_)): _*)

    def withBody(body: PBTransaction): PBCachedTransaction = tx match {
      case bbtx: PBCachedTransactionImplWithBodyBytes =>
        new PBCachedTransactionImplWithBodyBytes(body, bbtx.proofs)

      case btx: PBCachedTransactionImplWithBytes =>
        new PBCachedTransactionImplWithBodyBytes(body, btx.proofsCoeval())

      case _ =>
        new PBCachedTransactionImplWithBodyBytes(body, tx.transaction.proofs)
    }
  }

  private sealed trait PBSerializable {
    def serializedSize: Int
    def toBytes: Array[Byte]
  }

  private object PBSerializable {
    implicit class PBRawBytesSerializable(private val underlyingBytes: Array[Byte]) extends PBSerializable {
      override def serializedSize: Int =
        _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(underlyingBytes.length) + underlyingBytes.length
      override def toBytes: Array[Byte] = underlyingBytes
    }

    implicit class PBMessageSerializable(val underlyingMessage: GeneratedMessage) extends PBSerializable {
      private[this] val bytesCoeval = Coeval.evalOnce(PBUtils.encodeDeterministic(underlyingMessage))
      override def serializedSize: Int =
        _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(underlyingMessage.serializedSize) + underlyingMessage.serializedSize
      override def toBytes: Array[Byte] = bytesCoeval()
    }
  }

  private abstract class PBCachedTransactionImpl extends PBCachedTransaction {
    private[transaction] val transactionCoeval: Coeval[PBSignedTransaction]
    private[transaction] val bodyBytesCoeval: Coeval[Array[Byte]]
    private[transaction] val bytesCoeval: Coeval[Array[Byte]]

    override def transaction: PBSignedTransaction = transactionCoeval()
    override def bodyBytes: Array[Byte]           = bodyBytesCoeval()
    override def bytes: Array[Byte]               = bytesCoeval()
  }

  private class PBCachedTransactionImplWithBodyBytes(val underlying: PBSerializable, val proofs: Seq[ByteString]) extends PBCachedTransactionImpl {
    override private[transaction] val bodyBytesCoeval = Coeval(underlying.toBytes)
    override private[transaction] val bytesCoeval = Coeval.evalOnce {
      val bodyBytes = this.bodyBytesCoeval()

      val serializedSize = {
        val bodySize =
          if (bodyBytes.nonEmpty) 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(bodyBytes.length) + bodyBytes.length else 0
        val proofsSize = proofs.map { __item =>
          val __value = __item
          _root_.com.google.protobuf.CodedOutputStream.computeBytesSize(2, __value)
        }.sum
        bodySize + proofsSize
      }

      val outArray  = new Array[Byte](serializedSize)
      val _output__ = CodedOutputStream.newInstance(outArray)

      if (bodyBytes.nonEmpty) {
        _output__.writeTag(1, 2)
        // _output__.writeUInt32NoTag(__m.serializedSize)
        // __m.writeTo(_output__)
        _output__.writeUInt32NoTag(bodyBytes.length)
        _output__.write(bodyBytes, 0, bodyBytes.length)
      }
      proofs.foreach { __v =>
        val __m = __v
        _output__.writeBytes(2, __m)
      }
      _output__.flush()
      _output__.checkNoSpaceLeft()
      outArray
    }
    override private[transaction] val transactionCoeval: Coeval[PBSignedTransaction] = Coeval.evalOnce {
      val tx = underlying match {
        case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBTransaction]
        case _                                        => PBTransaction.parseFrom(underlying.toBytes)
      }
      PBSignedTransaction(Some(tx), proofs)
    }
  }

  private class PBCachedTransactionImplWithBytes(val underlying: PBSerializable) extends PBCachedTransactionImpl {
    override private[transaction] val transactionCoeval: Coeval[PBSignedTransaction] = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBSignedTransaction]
      case _                                        => PBSignedTransaction.parseFrom(underlying.toBytes)
    })

    override private[transaction] val bodyBytesCoeval: Coeval[Array[Byte]] = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable =>
        PBTransactionSerialization.unsignedBytes(ms.underlyingMessage.asInstanceOf[PBSignedTransaction].getTransaction)

      case _ =>
        val codedInputStream = CodedInputStream.newInstance(underlying.toBytes)
        var done             = false
        var result           = Option.empty[ByteString]
        while (!done) {
          val tag = codedInputStream.readTag()
          tag match {
            case 0 => done = true
            case 10 =>
              result = Some(codedInputStream.readBytes())
              done = true
            case tag => codedInputStream.skipField(tag)
          }
        }
        result.fold(transactionCoeval().getTransaction.toByteArray)(_.toByteArray)
    })

    private[transaction] val proofsCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBSignedTransaction].proofs
      case _ =>
        val _input__ = CodedInputStream.newInstance(underlying.toBytes)
        val __proofs = _root_.scala.collection.immutable.Vector.newBuilder[_root_.com.google.protobuf.ByteString]
        var _done__  = false
        while (!_done__) {
          val _tag__ = _input__.readTag()
          _tag__ match {
            case 0   => _done__ = true
            case 18  => __proofs += _input__.readBytes()
            case tag => _input__.skipField(tag)
          }
        }
        __proofs.result()
    })

    override private[transaction] val bytesCoeval: Coeval[Array[Byte]] =
      Coeval.evalOnce(new PBCachedTransactionImplWithBodyBytes(bodyBytesCoeval(), proofsCoeval()).bytes)
  }
}
