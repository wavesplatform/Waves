// GENERATED CAP'N PROTO FILE, DO NOT EDIT
package com.wavesplatform.account.capnp

object Recipient {
  // START Recipient implicits
  implicit val Recipient$$HasTypeId = new org.katis.capnproto.runtime.HasTypeId[Recipient] {
    def typeId: Long = -4093217595474627559L
  }
  implicit val Recipient$$HasStructSize = new org.katis.capnproto.runtime.HasStructSize[Recipient] {
    val structSize = new org.katis.capnproto.runtime.StructSize(1, 1)
  }
  implicit val Recipient$$StructFromSegment = new org.katis.capnproto.runtime.StructFromSegment[Recipient] {
    def readerFromSegment(segment: org.katis.capnproto.runtime.SegmentReader, dataOffset: Int, pointers: Int, dataSize: Int, pointerCount: Short, nestingLimit: Int): Recipient#Reader = {
      Recipient.Reader(segment, dataOffset, pointers, dataSize, pointerCount, nestingLimit)
    }
    def builderFromSegment(segment: org.katis.capnproto.runtime.SegmentBuilder, dataOffset: Int, pointers: Int, dataSize: Int, pointerCount: Short): Recipient#Builder = {
      Recipient.Builder(segment, dataOffset, pointers, dataSize, pointerCount)
    }
  }
  // END Recipient implicits

  object Recipient extends Recipient
  sealed class Recipient private() extends org.katis.capnproto.runtime.Struct { 
    val typeId: Long = -4093217595474627559L
    override val structSize: org.katis.capnproto.runtime.StructSize = new org.katis.capnproto.runtime.StructSize(1, 1)

    override type Reader = ReaderImpl
    override type Builder = BuilderImpl

    override val Builder: (org.katis.capnproto.runtime.SegmentBuilder, Int, Int, Int, Short) => Builder = new BuilderImpl(_, _, _, _, _)
    override val Reader: (org.katis.capnproto.runtime.SegmentReader, Int, Int, Int, Short, Int) => Reader = new ReaderImpl(_, _, _, _, _, _)

    class ReaderImpl(_segment: org.katis.capnproto.runtime.SegmentReader, _dataOffset: Int, _pointers: Int, _dataSize: Int, _pointerCount: Short, _nestingLimit: Int) extends super.ReaderBase(_segment, _dataOffset, _pointers, _dataSize, _pointerCount, _nestingLimit) {
      def chainId: Int = {
        this._getIntField(0)
      }
      private[Recipient] def address: org.katis.capnproto.runtime.Data#Reader = {
        _getPointerField[org.katis.capnproto.runtime.Data](0)
      }
      private[Recipient] def alias: org.katis.capnproto.runtime.Text#Reader = {
        _getPointerField[org.katis.capnproto.runtime.Text](0)
      }
      private[Recipient] def _whichIndex: Short = _getShortField(2)
      object has {
        def address: Boolean = {
          if (_getShortField(2) != 0) false
          else _pointerFieldIsNull(0)
        }
        def alias: Boolean = {
          if (_getShortField(2) != 1) false
          else _pointerFieldIsNull(0)
        }
      }
    }
    class BuilderImpl(_segment: org.katis.capnproto.runtime.SegmentBuilder, _dataOffset: Int, _pointers: Int, _dataSize: Int, _pointerCount: Short) extends super.BuilderBase(_segment, _dataOffset, _pointers, _dataSize, _pointerCount) {
      def chainId: Int = {
        this._getIntField(0)
      }
      def chainId_=(value: Int): Unit = {
        _setIntField(0, value)
      }
      private[Recipient] def address: org.katis.capnproto.runtime.Data#Builder = {
        _getPointerField[org.katis.capnproto.runtime.Data](0)
      }
      def address_=(value: org.katis.capnproto.runtime.Data#Reader): Unit = {
        _setShortField(2, 0)
        _setPointerField[org.katis.capnproto.runtime.Data](0, value)
      }
      private[Recipient] def alias: org.katis.capnproto.runtime.Text#Builder = {
        _getPointerField[org.katis.capnproto.runtime.Text](0)
      }
      def alias_=(value: org.katis.capnproto.runtime.Text#Reader): Unit = {
        _setShortField(2, 1)
        _setPointerField[org.katis.capnproto.runtime.Text](0, value)
      }
      def alias_=(value: String): Unit = {
        _setPointerField[org.katis.capnproto.runtime.Text](0, org.katis.capnproto.runtime.Text.Reader(value))
      }
      private[Recipient] def _whichIndex: Short = _getShortField(2)
      object has {
        def address: Boolean = {
          if (_getShortField(2) != 0) false
          else _pointerFieldIsNull(0)
        }
        def alias: Boolean = {
          if (_getShortField(2) != 1) false
          else _pointerFieldIsNull(0)
        }
      }
      object init {
        def address(size: Int): org.katis.capnproto.runtime.Data#Builder = {
          _setShortField(2, 0)
          _initPointerField[org.katis.capnproto.runtime.Data](0, size)
        }
        def alias(size: Int): org.katis.capnproto.runtime.Text#Builder = {
          _setShortField(2, 1)
          _initPointerField[org.katis.capnproto.runtime.Text](0, size)
        }
      }
    }
    object Address {
      def unapply(value: Recipient#Reader): Option[org.katis.capnproto.runtime.Data#Reader] = {
        if (value._whichIndex == 0) Some(value.address) else None
      }
      def unapply(value: Recipient#Builder): Option[org.katis.capnproto.runtime.Data#Builder] = {
        if (value._whichIndex == 0) Some(value.address) else None
      }
    }
    object Alias {
      def unapply(value: Recipient#Reader): Option[org.katis.capnproto.runtime.Text#Reader] = {
        if (value._whichIndex == 1) Some(value.alias) else None
      }
      def unapply(value: Recipient#Builder): Option[org.katis.capnproto.runtime.Text#Builder] = {
        if (value._whichIndex == 1) Some(value.alias) else None
      }
    }
  }

  val addr = new MessageBuilder()
}
