package com.wavesplatform.transaction.transfer

import java.nio.charset.StandardCharsets

sealed trait Attachment {
  def size: Int
}

object Attachment {
  final case class Num(value: Long) extends Attachment {
    override def size: Int = 8
  }

  final case class Bool(value: Boolean) extends Attachment {
    override def size: Int = 1
  }

  final case class Bin(value: Array[Byte]) extends Attachment {
    override def size: Int = value.length
  }

  final case class Str(value: String) extends Attachment {
    override lazy val size: Int = value.getBytes(StandardCharsets.UTF_8).length
  }

  final case object Empty extends Attachment {
    override def size: Int = 0
  }

  def fromBytes(bs: Array[Byte]): Attachment =
    if (bs.isEmpty) Empty else Bin(bs)

  implicit class AttachmentExt(private val a: Attachment) extends AnyVal {
    def asBytes: Array[Byte] = a match {
      case Bin(value) => value
      case Empty      => Array.emptyByteArray
      case _          => throw new IllegalArgumentException(s"Not a bytes attachment: $a")
    }
  }
}
