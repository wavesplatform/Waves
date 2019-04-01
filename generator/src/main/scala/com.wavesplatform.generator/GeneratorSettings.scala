package com.wavesplatform.generator

import java.net.InetSocketAddress

import cats.Show
import cats.implicits.showInterpolator
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.utils.EitherExt2

case class GeneratorSettings(chainId: String,
                             accounts: Seq[String],
                             sendTo: Seq[InetSocketAddress],
                             worker: Worker.Settings,
                             mode: Mode.Value,
                             narrow: NarrowTransactionGenerator.Settings,
                             wide: WideTransactionGenerator.Settings,
                             dynWide: DynamicWideTransactionGenerator.Settings,
                             multisig: MultisigTransactionGenerator.Settings,
                             oracle: OracleTransactionGenerator.Settings,
                             swarm: SmartGenerator.Settings) {
  val addressScheme: Char                        = chainId.head
  val privateKeyAccounts: Seq[KeyPair] = accounts.map(s => KeyPair.fromSeed(s).explicitGet())
}

object GeneratorSettings {
  implicit val toPrintable: Show[GeneratorSettings] = { x =>
    import x._

    val modeSettings: String = (mode match {
      case Mode.NARROW   => show"$narrow"
      case Mode.WIDE     => show"$wide"
      case Mode.DYN_WIDE => show"$dynWide"
      case Mode.MULTISIG => show"$multisig"
      case Mode.ORACLE   => show"$oracle"
      case Mode.SWARM    => show"$swarm"
    }).toString

    s"""network byte: $chainId
       |rich accounts:
       |  ${accounts.mkString("\n  ")}
       |recipient nodes:
       |  ${sendTo.mkString("\n  ")}
       |worker:
       |  ${show"$worker".split('\n').mkString("\n  ")}
       |mode: $mode
       |$mode settings:
       |  ${modeSettings.split('\n').mkString("\n  ")}""".stripMargin
  }
}
