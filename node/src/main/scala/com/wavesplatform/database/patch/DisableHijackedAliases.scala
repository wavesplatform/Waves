package com.wavesplatform.database.patch

import com.typesafe.scalalogging.StrictLogging
import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.utils._
import com.wavesplatform.database
import com.wavesplatform.database.{KeyTags, Keys, RW}
import com.wavesplatform.state.patch.PatchDataLoader
import play.api.libs.json.Json

case object DisableHijackedAliases extends PatchDataLoader with StrictLogging {
  val height: Int = AddressScheme.current.chainId.toChar match {
    case 'W' => 1060000
    case _   => 0
  }

  def collectLeases(rw: RW, disabledAliases: Set[Alias]): Unit = {
    rw.iterateOver(KeyTags.LeaseDetails) { e =>
      val ld = database.readLeaseDetails(e.getValue)
      ld.recipient match {
        case a: Alias if disabledAliases(a) =>
          logger.info(
            Json.stringify(
              Json.obj(
                "id"               -> Base58.encode(e.getKey.drop(6)),
                "amount"           -> ld.amount,
                "recipient"        -> ld.recipient.toString,
                "recipientAddress" -> ld.recipientAddress.toString,
                "senderAddress"    -> ld.sender.toAddress
              )
            )
          )
        case _ =>
      }
    }
  }

  def apply(rw: RW): Set[Alias] = {
    val aliases = readPatchData[Set[String]]().map(Alias.create(_).explicitGet())
    rw.put(Keys.disabledAliases, aliases)
    collectLeases(rw, aliases)
    aliases
  }

  def revert(rw: RW): Set[Alias] = {
    rw.put(Keys.disabledAliases, Set.empty[Alias])
    Set.empty[Alias]
  }
}
