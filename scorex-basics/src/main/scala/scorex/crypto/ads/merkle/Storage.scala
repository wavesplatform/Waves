package scorex.crypto.ads.merkle

import scorex.crypto.CryptographicHash.Digest

trait Storage {

  import Storage._

  def set(key: Key, value: Digest): Unit

  def get(key: Key): Option[Digest]

  def commit(): Unit

  def close(): Unit
}

object Storage {
  type Level = Int
  type Position = Long
  type Key = (Level, Position)
}