package settings

import java.net.InetSocketAddress

import cats.Show
import cats.implicits.showInterpolator
import com.wavesplatform.account.KeyPair
import com.wavesplatform.dexgen._
import com.wavesplatform.wallet.Wallet

case class GeneratorSettings(chainId: String,
                             richAccounts: Seq[String],
                             sendTo: Seq[InetSocketAddress],
                             matcherConfig: MatcherNodeSettings.Settings,
                             worker: Worker.Settings,
                             accountsDistribution: AccountsDistribution.Settings,
                             mode: Mode.Value,
                             dex: OrdersSettings.Settings) {
  val addressScheme: Char     = chainId.head
  val matcherNodePath: String = matcherConfig.endpoint

  val assetPairsNum: Int = dex.assets
  //accounts
  val richPrivateKeys: Seq[KeyPair] = richAccounts.map(s => KeyPair.fromSeed(s).right.get)

  def accounts(startNonce: Int, end: Int): Seq[KeyPair] =
    (startNonce to end).map { i =>
      Wallet.generateNewAccount(Array.emptyByteArray, i)
    }

  val validAccounts: Seq[KeyPair]   = accounts(1, accountsDistribution.valid)
  val invalidAccounts: Seq[KeyPair] = accounts(accountsDistribution.valid + 1, accountsDistribution.invalid)
  val fakeAccounts: Seq[KeyPair]    = accounts(accountsDistribution.valid + accountsDistribution.invalid + 1, accountsDistribution.fake)

}

object GeneratorSettings {
  implicit val toPrintable: Show[GeneratorSettings] = { x =>
    import x._

    val modeSettings: String = (mode match {
      case Mode.DEX => show"$dex"
    }).toString

    s"""network byte: $chainId
       |rich accounts:
       |  ${richAccounts.mkString("\n  ")}
       |recipient nodes:
       |  ${sendTo.mkString("\n  ")}
       |accounts disribution:
       |  ${show"$accountsDistribution".split('\n').mkString("\n ")}
       |worker:
       |  ${show"$worker".split('\n').mkString("\n  ")}
       |mode: $mode
       |$mode settings:
       |  ${modeSettings.split('\n').mkString("\n  ")}""".stripMargin
  }
}
