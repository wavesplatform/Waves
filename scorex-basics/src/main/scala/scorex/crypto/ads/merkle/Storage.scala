package scorex.crypto.ads.merkle

import scorex.crypto.CryptographicHash.Digest

import scala.util.Try

trait Storage {

  import Storage._

  def set(key: Key, value: Digest): Try[Digest]

  def get(key: Key): Option[Digest]

  def commit(): Unit
}

object Storage {
  type Level = Int
  type Position = Int
  type Key = (Level, Position)

}