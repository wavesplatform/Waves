package scorex.perma.merkle

trait CryptographicHash {
  import CryptographicHash._

  def hash(byteSequence: Message): Digest
}

object CryptographicHash {
  type Message = Seq[Byte]
  type Digest = Vector[Byte]
}
