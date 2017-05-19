package scorex.network

trait ConnectedPeer {
  def nonce: Long

  def blacklist(): Unit

  def suspect(): Unit
}
