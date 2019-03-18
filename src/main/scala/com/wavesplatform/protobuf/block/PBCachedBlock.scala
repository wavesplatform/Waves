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
        var __size = 0
        if (header.isDefined) {
          val __value = header.get
          __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.length) + __value.length
        }

        {
          val __value = signature
          if (__value.nonEmpty) {
            __size += _root_.com.google.protobuf.CodedOutputStream.computeByteArraySize(2, __value)
          }
        }
        transactions.foreach { __item =>
          val __value = __item
          __size += 1 + _root_.com.google.protobuf.CodedOutputStream.computeUInt32SizeNoTag(__value.serializedSize) + __value.serializedSize
        }
        __size
      }

      val outArray  = new Array[Byte](serializedSize)
      val _output__ = CodedOutputStream.newInstance(outArray)

      header.foreach { __v =>
        val __m = __v
        _output__.writeTag(1, 2)
        _output__.writeByteArrayNoTag(__m)
      }

      {
        val __v = signature
        if (__v.nonEmpty) {
          _output__.writeByteArray(2, __v)
        }
      }

      transactions.foreach { __v =>
        val __m = __v
        _output__.writeTag(3, 2)
        _output__.writeByteArrayNoTag(__m.toBytes)
      }

      _output__.flush()
      _output__.checkNoSpaceLeft()
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
        val _input__    = CodedInputStream.newInstance(underlying.toBytes)
        var __header    = Array.emptyByteArray
        var __signature = Array.emptyByteArray
        var _done__     = false
        while (!_done__) {
          val _tag__ = _input__.readTag()
          _tag__ match {
            case 0 => _done__ = true
            case 10 =>
              __header = _input__.readByteArray()
            case 18 =>
              __signature = _input__.readByteArray()
            case tag => _input__.skipField(tag)
          }
        }
        (__header, __signature)
    })
    override private[block] val bytesCoeval = Coeval.evalOnce(underlying.toBytes)
    override private[block] val transactionsBytesCoeval = Coeval.evalOnce(underlying match {
      case ms: PBSerializable.PBMessageSerializable => ms.underlyingMessage.asInstanceOf[PBBlock].transactions.map(PBUtils.encodeDeterministic)
      case _ =>
        val _input__       = CodedInputStream.newInstance(underlying.toBytes)
        val __transactions = _root_.scala.collection.immutable.Vector.newBuilder[Array[Byte]]
        var _done__        = false
        while (!_done__) {
          val _tag__ = _input__.readTag()
          _tag__ match {
            case 0 => _done__ = true
            case 26 =>
              __transactions += _input__.readByteArray()
            case tag => _input__.skipField(tag)
          }
        }
        __transactions.result()
    })
  }
}
