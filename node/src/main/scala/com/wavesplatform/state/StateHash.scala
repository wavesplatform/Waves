package com.wavesplatform.state

import com.google.common.base.CaseFormat
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.StateHash.SectionId
import org.bouncycastle.util.encoders.Hex
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{Json, OWrites}

final case class StateHash(totalHash: ByteStr, sectionHashes: Map[SectionId.Value, ByteStr]) {
  require(totalHash.arr.length == crypto.DigestLength && sectionHashes.values.forall(_.arr.length == crypto.DigestLength))
}

object StateHash {
  object SectionId extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship, Alias = Value
  }

  private val converter = CaseFormat.UPPER_CAMEL.converterTo(CaseFormat.LOWER_CAMEL)
  implicit val writes: OWrites[StateHash] = OWrites { sh =>
    def lowerCamel(sectionId: SectionId.Value): String = converter.convert(sectionId.toString)
    def toHexString(bs: ByteStr)                       = Hex.toHexString(bs.arr)

    Json.obj("stateHash" -> toHexString(sh.totalHash)) ++ Json.obj(
      SectionId.values.toSeq
        .map(id => s"${lowerCamel(id)}Hash" -> (toHexString(sh.sectionHashes.getOrElse(id, StateHashBuilder.EmptySectionHash)): JsValueWrapper))*
    )
  }
}
