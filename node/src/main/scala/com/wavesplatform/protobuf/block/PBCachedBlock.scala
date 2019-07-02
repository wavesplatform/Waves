package com.wavesplatform.protobuf.block
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.PBSerializable
import com.wavesplatform.protobuf.transaction.PBCachedTransaction
import com.wavesplatform.protobuf.transaction.PBCachedTransaction.PBCachedTransactionSerializable
import com.wavesplatform.protobuf.utils.PBUtils
import monix.eval.Coeval

sealed trait PBCachedBlock {
  def block: PBBlock

  def header: PBBlock.Header
  def headerBytes: Array[Byte]

  def signature: Array[Byte]

  def transactions: Seq[PBCachedTransaction]
  def bytes: Array[Byte]
}

object PBCachedBlock {
  implicit def apply(block: PBBlock): PBCachedBlock = new PBCachedBlockImplWithBytes(block)

  def create(header: PBBlock.Header, signature: Array[Byte] = Array.emptyByteArray, transactions: Seq[PBCachedTransaction] = Nil): PBCachedBlock =
    new PBCachedBlockImplWithHeaderAndTransactions(header, signature, transactions)

  def fromBytes(bytes: Array[Byte]): PBCachedBlock = new PBCachedBlockImplWithBytes(bytes)

  implicit class PBCachedBlockImplicitOps(private val block: PBCachedBlock) extends AnyVal {
    def withSignature(signature: ByteStr): PBCachedBlock = block match {
      case whtxs: PBCachedBlockImplWithHeaderAndTransactions =>
        new PBCachedBlockImplWithHeaderAndTransactions(whtxs.headerV, signature, whtxs.transactionsSz)

      case _ =>
        new PBCachedBlockImplWithHeaderAndTransactions(block.headerBytes, signature, block.transactions)
    }

    def withTransactions(transactions: Seq[PBCachedTransaction]): PBCachedBlock = block match {
      case whtxs: PBCachedBlockImplWithHeaderAndTransactions =>
        new PBCachedBlockImplWithHeaderAndTransactions(whtxs.headerV, whtxs.signatureBs, transactions)

      case _ =>
        new PBCachedBlockImplWithHeaderAndTransactions(block.headerBytes, block.signature, transactions)
    }
  }

  private abstract class PBCachedBlockImpl extends PBCachedBlock {
    private[block] val blockCoeval: Coeval[PBBlock]
    private[block] val headerCoeval: Coeval[PBBlock.Header]
    private[block] val headerBytesCoeval: Coeval[(Array[Byte], Array[Byte])]
    private[block] val transactionsCoeval: Coeval[Seq[PBCachedTransaction]]
    private[block] val bytesCoeval: Coeval[Array[Byte]]

    override def block: PBBlock = blockCoeval()

    override def header: Block.Header = headerCoeval()

    override def headerBytes: Array[Byte] = headerBytesCoeval()._1

    override def signature: Array[Byte] = headerBytesCoeval()._2

    override def transactions: Seq[PBCachedTransaction] = transactionsCoeval()

    override def bytes: Array[Byte] = bytesCoeval()
  }

  private class PBCachedBlockImplWithHeaderAndTransactions(val headerV: PBSerializable,
                                                           val signatureBs: Array[Byte],
                                                           val transactionsSz: Seq[PBCachedTransaction])
      extends PBCachedBlockImpl {

    override private[block] val headerCoeval = Coeval.evalOnce(headerV match {
      case serializable: PBSerializable.PBMessageSerializable => serializable.underlyingMessage.asInstanceOf[PBBlock.Header]
      case _ => PBBlock.Header.parseFrom(headerV.toBytes)
    })
    override private[block] val headerBytesCoeval = Coeval.evalOnce((headerV.toBytes, signatureBs))

    override private[block] val transactionsCoeval = Coeval.evalOnce(transactionsSz)

    override private[block] val blockCoeval = Coeval.evalOnce {
      PBBlock(Some(headerCoeval()), PBUtils.toByteStringUnsafe(signatureBs), transactionsCoeval().map(_.transaction))
    }
    override private[block] val bytesCoeval = Coeval.evalOnce {
      val header = Option(headerBytesCoeval()._1).filter(_.nonEmpty)

      val serializedSize = {
        var size = 0
        if (header.isDefined) {
          val headerValue = header.get
          size += 1 + CodedOutputStream.computeUInt32SizeNoTag(headerValue.length) + headerValue.length
        }

        {
          if (signatureBs.nonEmpty) {
            size += CodedOutputStream.computeByteArraySize(2, signatureBs)
          }
        }
        transactionsSz.foreach { tx =>
          size += 1 + CodedOutputStream.computeUInt32SizeNoTag(tx.serializedSize) + tx.serializedSize
        }
        size
      }

      val outArray = new Array[Byte](serializedSize)
      val outputStream = CodedOutputStream.newInstance(outArray)

      header.foreach { header =>
        outputStream.writeTag(1, 2)
        outputStream.writeByteArrayNoTag(header)
      }

      {
        if (signatureBs.nonEmpty) {
          outputStream.writeByteArray(2, signatureBs)
        }
      }

      transactionsSz.foreach { tx =>
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

    override private[block] val headerCoeval = blockCoeval.map(_.getHeader)
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
    override private[block] val transactionsCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable =>
        ms.underlyingMessage.asInstanceOf[PBBlock].transactions.map(PBCachedTransaction(_))

      case _ =>
        val inputStream = CodedInputStream.newInstance(underlying.toBytes)
        val transactions = Vector.newBuilder[PBCachedTransaction]
        var done = false
        while (!done) {
          inputStream.readTag() match {
            case 0 => done = true
            case 26 =>
              transactions += PBCachedTransaction.fromBytes(inputStream.readByteArray())
            case tag => inputStream.skipField(tag)
          }
        }
        transactions.result()
    })
  }
}
