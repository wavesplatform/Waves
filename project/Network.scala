import sbt._
import complete.DefaultParsers._
import sbt.complete._

sealed abstract class Network(val suffix: String) {
  lazy val packageSuffix: String = if (suffix == Mainnet.suffix) "" else "-" + suffix
  override val toString: String  = suffix
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

object Mainnet  extends Network("mainnet")
object Testnet  extends Network("testnet")
object Devnet   extends Network("devnet")
object Stagenet extends Network("stagenet")
