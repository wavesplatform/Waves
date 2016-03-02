package scorex.network

import java.nio.ByteOrder

import akka.util.ByteString

import scala.annotation.tailrec

/**
  * Taken from https://stackoverflow.com/questions/30665811/scala-tcp-packet-frame-using-akka
  */

trait Buffering {

  //1 MB max packet size
  val MAX_PACKET_LEN: Int = 1024*1024

  /**
    * Extracts complete packets of the specified length, preserving remainder
    * data. If there is no complete packet, then we return an empty list. If
    * there are multiple packets available, all packets are extracted, Any remaining data
    * is returned to the caller for later submission
    * @param data A list of the packets extracted from the raw data in order of receipt
    * @return A list of ByteStrings containing extracted packets as well as any remaining buffer data not consumed
    */

  def getPacket(data: ByteString): (List[ByteString], ByteString) = {

    val headerSize = 4

    @tailrec
    def multiPacket(packets: List[ByteString], current: ByteString): (List[ByteString], ByteString) = {
      if (current.length < headerSize) {
        (packets.reverse, current)
      } else {
        val len = current.iterator.getInt(ByteOrder.BIG_ENDIAN)
        if (len > MAX_PACKET_LEN || len < 0) throw new Exception(s"Invalid packet length: $len")
        if (current.length < len + headerSize) {
          (packets.reverse, current)
        } else {
          val rem = current drop headerSize // Pop off header
          val (front, back) = rem.splitAt(len) // Front contains a completed packet, back contains the remaining data
          // Pull of the packet and recurse to see if there is another packet available
          multiPacket(front :: packets, back)
        }
      }
    }
    multiPacket(List[ByteString](), data)
  }
}