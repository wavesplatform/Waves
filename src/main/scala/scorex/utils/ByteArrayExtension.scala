package scorex.utils


object ByteArrayExtension {

  def compare(buffer1: Array[Byte], buffer2: Array[Byte]): Int = {
    ByteArray.compare(buffer1, buffer2)
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
