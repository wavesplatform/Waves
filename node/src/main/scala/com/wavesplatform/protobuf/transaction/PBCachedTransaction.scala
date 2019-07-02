package com.wavesplatform.protobuf.transaction
import com.google.protobuf.{ByteString, CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.PBSerializable
import com.wavesplatform.protobuf.utils.PBUtils
import com.wavesplatform.transaction.{FastHashId, Proofs}
import monix.eval.Coeval

sealed trait PBCachedTransaction {
  def transaction: SignedTransaction

  def proofs: Seq[Array[Byte]]
  def bodyBytes: Array[Byte] // Not equals to vanilla tx bodyBytes
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
  implicit def apply(tx: PBTransaction): PBCachedTransaction       = new PBCachedTransactionImplWithBodyBytes(tx, Nil)
  implicit def apply(tx: PBSignedTransaction): PBCachedTransaction = new PBCachedTransactionImplWithBytes(tx)
  def fromBytes(bytes: Array[Byte]): PBCachedTransaction           = new PBCachedTransactionImplWithBytes(bytes)
  def fromBodyBytes(bodyBytes: Array[Byte]): PBCachedTransaction   = new PBCachedTransactionImplWithBodyBytes(bodyBytes, Nil)

  implicit def toSignedTransaction(tx: PBCachedTransaction): PBSignedTransaction =
    tx.transaction

  implicit class PBCachedTransactionImplicitOps(private val tx: PBCachedTransaction) extends AnyVal {
    def withProofs(proofs: ByteString*): PBCachedTransaction = tx match {
      case bbtx: PBCachedTransactionImplWithBodyBytes =>
        new PBCachedTransactionImplWithBodyBytes(bbtx.underlying, proofs.map(_.toByteArray))

      case _ =>
        new PBCachedTransactionImplWithBodyBytes(tx.bodyBytes, proofs.map(_.toByteArray))
    }

    def withProofs(proof1: ByteStr, proofs: ByteStr*): PBCachedTransaction = withProofs((proof1 +: proofs).map(PBUtils.toByteStringUnsafe(_)): _*)

    def withProofs(proofs: Proofs): PBCachedTransaction = withProofs(proofs.proofs.map(PBUtils.toByteStringUnsafe(_)): _*)

    def withBody(body: PBTransaction): PBCachedTransaction = tx match {
      case bbtx: PBCachedTransactionImplWithBodyBytes =>
        new PBCachedTransactionImplWithBodyBytes(body, bbtx.proofsBs)

      case btx: PBCachedTransactionImplWithBytes =>
        new PBCachedTransactionImplWithBodyBytes(body, btx.proofsCoeval())

      case _ =>
        new PBCachedTransactionImplWithBodyBytes(body, tx.transaction.proofs.map(_.toByteArray))
    }
  }

  implicit class PBCachedTransactionSerializable(private[protobuf] val transaction: PBCachedTransaction) extends PBSerializable {
    override def serializedSize: Int = transaction.bytes.length

    override def toBytes: Array[Byte] = transaction.bytes
  }

  private abstract class PBCachedTransactionImpl extends PBCachedTransaction {
    private[transaction] val transactionCoeval: Coeval[PBSignedTransaction]
    private[transaction] val proofsCoeval: Coeval[Seq[Array[Byte]]]
    private[transaction] val bodyBytesCoeval: Coeval[Array[Byte]]
    private[transaction] val bytesCoeval: Coeval[Array[Byte]]

    override def transaction: PBSignedTransaction = transactionCoeval()

    override def proofs: Seq[Array[Byte]] = proofsCoeval()
    override def bodyBytes: Array[Byte]           = bodyBytesCoeval()
    override def bytes: Array[Byte]               = bytesCoeval()
  }

  private class PBCachedTransactionImplWithBodyBytes(val underlying: PBSerializable, val proofsBs: Seq[Array[Byte]]) extends PBCachedTransactionImpl {
    override private[transaction] val proofsCoeval = Coeval.evalOnce(proofsBs)
    override private[transaction] val bodyBytesCoeval = Coeval(underlying.toBytes)
    override private[transaction] val bytesCoeval = Coeval.evalOnce {
      val bodyBytes = this.bodyBytesCoeval()

      val serializedSize = {
        val bodySize =
          if (bodyBytes.nonEmpty) 1 + CodedOutputStream.computeUInt32SizeNoTag(bodyBytes.length) + bodyBytes.length else 0
        val proofsSize = proofsBs.map { proof =>
          CodedOutputStream.computeByteArraySize(2, proof)
        }.sum
        bodySize + proofsSize
      }

      val outArray = new Array[Byte](serializedSize)
      val outputStream = CodedOutputStream.newInstance(outArray)

      if (bodyBytes.nonEmpty) {
        outputStream.writeTag(1, 2)
        // _output__.writeUInt32NoTag(__m.serializedSize)
        // __m.writeTo(_output__)
        outputStream.writeUInt32NoTag(bodyBytes.length)
        outputStream.write(bodyBytes, 0, bodyBytes.length)
      }
      proofsBs.foreach { proof =>
        outputStream.writeByteArray(2, proof)
      }
      outputStream.flush()
      outputStream.checkNoSpaceLeft()
      outArray
    }
    override private[transaction] val transactionCoeval: Coeval[PBSignedTransaction] = Coeval.evalOnce {
      val tx = underlying match {
        case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBTransaction]
        case _                                        => PBTransaction.parseFrom(underlying.toBytes)
      }
      PBSignedTransaction(Some(tx), proofsBs.map(PBUtils.toByteStringUnsafe))
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
          codedInputStream.readTag() match {
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
      case ms: PBSerializable.PBMessageSerializable =>
        ms.underlyingMessage.asInstanceOf[PBSignedTransaction].proofs.map(PBUtils.toByteArrayUnsafe)
      case _ =>
        val inputStream = CodedInputStream.newInstance(underlying.toBytes)
        val proofsBuilder = Vector.newBuilder[Array[Byte]]
        var done = false
        while (!done) {
          inputStream.readTag() match {
            case 0 => done = true
            case 18 => proofsBuilder += inputStream.readByteArray()
            case tag => inputStream.skipField(tag)
          }
        }
        proofsBuilder.result()
    })

    override private[transaction] val bytesCoeval: Coeval[Array[Byte]] =
      Coeval.evalOnce(new PBCachedTransactionImplWithBodyBytes(bodyBytesCoeval(), proofsCoeval()).bytes)
  }
}
