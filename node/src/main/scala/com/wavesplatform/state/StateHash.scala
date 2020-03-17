package com.wavesplatform.state

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.state.StateHash.SectionId
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.{Json, OWrites}

case class StateHash(totalHash: ByteStr, bySection: Map[SectionId.Value, ByteStr]) {
  require(totalHash.length == crypto.DigestSize && bySection.values.forall(_.length == crypto.DigestSize))
}

object StateHash {
  object SectionId extends Enumeration {
    val WavesBalance, AssetBalance, DataEntry, Alias, AccountScript, AssetScript, LeaseBalance, LeaseStatus, Sponsorship = Value
  }

  implicit val writes: OWrites[StateHash] = OWrites { hash =>
    def decapitalize(sectionId: SectionId.Value): String = {
      val str = sectionId.toString
      str.head.toLower +: str.tail
    }

    Json.obj("stateHash" -> hash.totalHash.toString) ++ Json.obj(
      SectionId.values.toSeq
        .map(id => s"${decapitalize(id)}Hash" -> (hash.bySection.getOrElse(id, StateHashBuilder.EmptySectionHash).toString: JsValueWrapper)): _*
    )
  }
}
