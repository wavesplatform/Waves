package com.wavesplatform.protobuf.block
import com.google.protobuf.{ByteString, CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.PBSerializable
import com.wavesplatform.protobuf.transaction.PBSignedTransaction
import com.wavesplatform.protobuf.utils.PBUtils
import monix.eval.Coeval

trait PBCachedBlock {
  def block: PBBlock
  def headerBytes: Array[Byte]
  def signatureBytes: Array[Byte]
  def transactionsBytes: Seq[Array[Byte]]
  def bytes: Array[Byte]
}

object PBCachedBlock {
  implicit def apply(block: PBBlock): PBCachedBlock = new PBCachedBlockImplWithBytes(block)
  def create(header: PBBlock.Header, signature: Array[Byte] = Array.emptyByteArray, transactions: Seq[PBSignedTransaction] = Nil): PBCachedBlock =
    new PBCachedBlockImplWithHeaderAndTransactions(header, signature, transactions.map(tx => tx: PBSerializable))
  def fromBytes(bytes: Array[Byte]): PBCachedBlock = new PBCachedBlockImplWithBytes(bytes)

  implicit class PBCachedBlockImplicitOps(private val block: PBCachedBlock) extends AnyVal {
    def withSignature(signature: ByteStr): PBCachedBlock = block match {
      case whtxs: PBCachedBlockImplWithHeaderAndTransactions =>
        new PBCachedBlockImplWithHeaderAndTransactions(whtxs.header, signature, whtxs.transactions)

      case _ =>
        new PBCachedBlockImplWithHeaderAndTransactions(block.headerBytes, signature, block.transactionsBytes.map(tx => tx: PBSerializable))
    }

    def withTransactions(transactions: Seq[PBSignedTransaction]): PBCachedBlock = block match {
      case whtxs: PBCachedBlockImplWithHeaderAndTransactions =>
        new PBCachedBlockImplWithHeaderAndTransactions(whtxs.header, whtxs.signature, transactions.map(tx => tx: PBSerializable))

      case _ =>
        new PBCachedBlockImplWithHeaderAndTransactions(block.headerBytes, block.signatureBytes, transactions.map(tx => tx: PBSerializable))
    }
  }

  private abstract class PBCachedBlockImpl extends PBCachedBlock {
    private[block] val blockCoeval: Coeval[PBBlock]
    private[block] val headerBytesCoeval: Coeval[(Array[Byte], Array[Byte])]
    private[block] val transactionsBytesCoeval: Coeval[Seq[Array[Byte]]]
    private[block] val bytesCoeval: Coeval[Array[Byte]]

    override def block: PBBlock                      = blockCoeval()
    override def headerBytes: Array[Byte]            = headerBytesCoeval()._1
    override def signatureBytes: Array[Byte]         = headerBytesCoeval()._2
    override def transactionsBytes: Seq[Array[Byte]] = transactionsBytesCoeval()
    override def bytes: Array[Byte]                  = bytesCoeval()
  }

  private class PBCachedBlockImplWithHeaderAndTransactions(val header: PBSerializable,
                                                           val signature: Array[Byte],
                                                           val transactions: Seq[PBSerializable])
      extends PBCachedBlockImpl {

    override private[block] val blockCoeval = Coeval.evalOnce {
      val h = header match {
        case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBBlock.Header]
        case _                                        => PBBlock.Header.parseFrom(header.toBytes)
      }

      val txs = transactions.collect {
        case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBSignedTransaction]
        case tx                                       => PBSignedTransaction.parseFrom(tx.toBytes)
      }

      PBBlock(Some(h), ByteString.copyFrom(signature), txs)
    }
    override private[block] val headerBytesCoeval       = Coeval.evalOnce((header.toBytes, signature))
    override private[block] val transactionsBytesCoeval = Coeval.evalOnce(transactions.map(_.toBytes))
    override private[block] val bytesCoeval = Coeval.evalOnce {
      val header = Option(headerBytesCoeval()._1).filter(_.nonEmpty)

      val serializedSize = {
        var size = 0
        if (header.isDefined) {
          val headerValue = header.get
          size += 1 + CodedOutputStream.computeUInt32SizeNoTag(headerValue.length) + headerValue.length
        }

        {
          if (signature.nonEmpty) {
            size += CodedOutputStream.computeByteArraySize(2, signature)
          }
        }
        transactions.foreach { tx =>
          size += 1 + CodedOutputStream.computeUInt32SizeNoTag(tx.serializedSize) + tx.serializedSize
        }
        size
      }

      val outArray  = new Array[Byte](serializedSize)
      val outputStream = CodedOutputStream.newInstance(outArray)

      header.foreach { header =>
        outputStream.writeTag(1, 2)
        outputStream.writeByteArrayNoTag(header)
      }

      {
        if (signature.nonEmpty) {
          outputStream.writeByteArray(2, signature)
        }
      }

      transactions.foreach { tx =>
        outputStream.writeTag(3, 2)
        outputStream.writeByteArrayNoTag(tx.toBytes)
      }

      outputStream.flush()
      outputStream.checkNoSpaceLeft()
      outArray
    }
  }

  private class PBCachedBlockImplWithBytes(val underlying: PBSerializable) extends PBCachedBlockImpl {
    override private[block] val blockCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBBlock]
      case _                                        => PBBlock.parseFrom(underlying.toBytes)
    })
    override private[block] val headerBytesCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable =>
        val block = ms.underlyingMessage.asInstanceOf[PBBlock]
        (block.getHeader.toByteArray, block.signature.toByteArray)

      case _ =>
        val inputStream = CodedInputStream.newInstance(underlying.toBytes)
        var header = Array.emptyByteArray
        var signature = Array.emptyByteArray
        var done = false
        while (!done) {
          inputStream.readTag() match {
            case 0 => done = true
            case 10 =>
              header = inputStream.readByteArray()
            case 18 =>
              signature = inputStream.readByteArray()
            case tag => inputStream.skipField(tag)
          }
        }
        (header, signature)
    })
    override private[block] val bytesCoeval = Coeval.evalOnce(underlying.toBytes)
    override private[block] val transactionsBytesCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBBlock].transactions.map(PBUtils.encodeDeterministic)
      case _ =>
        val inputStream = CodedInputStream.newInstance(underlying.toBytes)
        val transactions = Vector.newBuilder[Array[Byte]]
        var done = false
        while (!done) {
          inputStream.readTag() match {
            case 0 => done = true
            case 26 =>
              transactions += inputStream.readByteArray()
            case tag => inputStream.skipField(tag)
          }
        }
        transactions.result()
    })
  }
}
