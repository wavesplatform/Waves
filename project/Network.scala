import sbt.*
import complete.DefaultParsers.*
import sbt.complete.*

sealed abstract class Network(val suffix: String) {
  lazy val packageSuffix: String = if (suffix == Mainnet.suffix) "" else "-" + suffix
//  override val toString: String  = suffix
}

object Network {
  private val oneNetwork: Parser[String]  = (Mainnet.suffix: Parser[String]) | Testnet.suffix | Stagenet.suffix | Devnet.suffix
  val networkParser: Parser[Seq[Network]] = (Space ~> oneNetwork.map(apply)).+
  def apply(v: String): Network = v match {
    case Testnet.suffix  => Testnet
    case Devnet.suffix   => Devnet
    case Stagenet.suffix => Stagenet
    case _               => Mainnet
  }

  def default(): Network = sys.props.get("network").fold[Network](Mainnet)(apply)
}

case object Mainnet  extends Network("mainnet")
case object Testnet  extends Network("testnet")
case object Devnet   extends Network("devnet")
case object Stagenet extends Network("stagenet")
