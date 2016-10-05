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

}
