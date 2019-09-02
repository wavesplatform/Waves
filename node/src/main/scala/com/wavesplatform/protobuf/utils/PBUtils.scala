package com.wavesplatform.protobuf.utils
import com.google.protobuf.CodedOutputStream
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.utils.ScorexLogging
import scalapb.GeneratedMessage

import scala.util.control.NonFatal

object PBUtils extends ScorexLogging {
  def encodeDeterministic(msg: GeneratedMessage): Array[Byte] = {
    val outArray     = new Array[Byte](msg.serializedSize)
    val outputStream = CodedOutputStream.newInstance(outArray)

    outputStream.useDeterministicSerialization() // Adds this
    msg.writeTo(outputStream)

    try outputStream.checkNoSpaceLeft()
    catch {
      case NonFatal(e) =>
        log.error(s"Error serializing PB message: $msg (bytes = ${ByteStr(msg.toByteArray)})", e)
        throw e
    }

    outArray
  }
}
