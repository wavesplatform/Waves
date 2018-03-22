package settings

import java.net.InetSocketAddress

import cats.Show
import cats.implicits.showInterpolator
import com.wavesplatform.generator._
import com.wavesplatform.state2.ByteStr
import scorex.account.PrivateKeyAccount
import scorex.utils.randomBytes


case class GeneratorSettings(chainId: String,
                             richAccounts: Seq[String],
                             sendTo: Seq[InetSocketAddress],
                             matcherConfig: MatcherNodeSettings.Settings,
                             worker: Worker.Settings,
                             accountsDistribution: AccountsDistribution.Settings,
                             mode: Mode.Value,
                             dex: OrdersSettings.Settings
                            ) {
  val addressScheme: Char = chainId.head
  val matcherNodePath: String = matcherConfig.endpoint

  val assetPairsNum: Int = dex.assets
  //accounts
  val richPrivateKeyAccounts: Seq[PrivateKeyAccount] = richAccounts.map(s => PrivateKeyAccount.fromSeed(s).right.get)

  def accounts(n: Int): Seq[PrivateKeyAccount] = {
    (1 to n).map { _ => PrivateKeyAccount.fromSeed(ByteStr(randomBytes(64)).base58).right.get }
  }

  val validAccounts: Seq[PrivateKeyAccount] = accounts(accountsDistribution.valid)
  val invalidAccounts: Seq[PrivateKeyAccount] = accounts(accountsDistribution.invalid)
  val fakeAccounts: Seq[PrivateKeyAccount] = accounts(accountsDistribution.fake)


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
