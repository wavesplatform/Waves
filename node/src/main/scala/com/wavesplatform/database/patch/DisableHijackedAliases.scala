package com.wavesplatform.database.patch

import com.wavesplatform.account.{AddressScheme, Alias}
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.database.{Keys, RW}
import com.wavesplatform.state.patch.PatchDataLoader
import com.wavesplatform.state.Height

case object DisableHijackedAliases extends PatchDataLoader {
  val height: Height = AddressScheme.current.chainId.toChar match {
    case 'W' => Height(1060000)
    case _   => Height(0)
  }

  def apply(rw: RW): Set[Alias] = {
    val aliases = readPatchData[Set[String]]().map(Alias.create(_).explicitGet())
    rw.put(Keys.disabledAliases, aliases)
    aliases
  }

  def revert(rw: RW): Set[Alias] = {
    rw.put(Keys.disabledAliases, Set.empty[Alias])
    Set.empty[Alias]
  }
}
