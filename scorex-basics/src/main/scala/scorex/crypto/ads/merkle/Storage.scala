package scorex.crypto.ads.merkle

trait Storage[Key, Value] {

  def set(key: Key, value: Value): Unit

  def get(key: Key): Option[Value]

  def commit(): Unit

  def close(): Unit
}