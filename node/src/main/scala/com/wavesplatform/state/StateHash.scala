package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.StateHash.SectionId
import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{Json, OWrites}

final case class StateHash(totalHash: ByteStr, sectionHashes: Map[SectionId.Value, ByteStr]) {
  require(totalHash.length == crypto.DigestSize && sectionHashes.values.forall(_.length == crypto.DigestSize))
}

object StateHash {
  object SectionId extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias = Value
  }

  implicit val writes: OWrites[StateHash] = OWrites { sh =>
    def decapitalize(sectionId: SectionId.Value): String = {
      val str = sectionId.toString
      str.head.toLower +: str.tail
    }

    def toHexString(bs: ByteStr) = Hex.toHexString(bs)

    Json.obj("stateHash" -> toHexString(sh.totalHash)) ++ Json.obj(
      SectionId.values.toSeq
        .map(id => s"${decapitalize(id)}Hash" -> (toHexString(sh.sectionHashes.getOrElse(id, StateHashBuilder.EmptySectionHash)): JsValueWrapper)): _*
    )
  }
}
