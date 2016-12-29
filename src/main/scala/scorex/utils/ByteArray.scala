package scorex.utils


object ByteArray {

  def compare(buffer1: Array[Byte], buffer2: Array[Byte]): Int = {
    if (buffer1 sameElements buffer2) {
      return 0
    }
    val end1: Int = if(buffer1.length < buffer2.length) buffer1.length else buffer2.length
    var i: Int = 0
    while (i < end1) {
      val a: Int = buffer1(i) & 0xff
      val b: Int = buffer2(i) & 0xff
      if (a != b) {
        return a - b
      }
      i = i + 1
    }
    buffer1.length - buffer2.length
  }

  def compare(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Int = {
    if (buffer1.isEmpty && buffer2.isEmpty) 0
    else if (buffer1.isEmpty) -1
    else if (buffer2.isEmpty) 1
    else compare(buffer1.get, buffer2.get)
  }

  def sameOption(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Boolean = {
    compare(buffer1, buffer2) == 0
  }
}
