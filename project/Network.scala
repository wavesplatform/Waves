sealed abstract class Network(val suffix: String) {
  lazy val packageSuffix = if (suffix == Mainnet.suffix) "" else "-" + suffix
  override val toString  = suffix
}

object Network {
  def apply(v: Option[String]): Network = v match {
    case Some(Testnet.suffix)  => Testnet
    case Some(Devnet.suffix)   => Devnet
    case Some(Stagenet.suffix) => Stagenet
    case _                     => Mainnet
  }
}

object Mainnet  extends Network("mainnet")
object Testnet  extends Network("testnet")
object Devnet   extends Network("devnet")
object Stagenet extends Network("stagenet")
