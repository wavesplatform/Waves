package com.wavesplatform.features.api

import com.wavesplatform.api.http.FinalityApiRoute.given
import com.wavesplatform.state.{GenerationPeriod, Height}
import com.wavesplatform.test.*
import play.api.libs.json.Json

class FinalityStatusSpec extends FreeSpec {
  private val activation = Height(7)

  "parse reads back the periods FinalityApiRoute writes" in {
    forAll(Table("height", 7, 10, 11, 13, 14, 20)) { h =>
      val current = GenerationPeriod.from(Height(h), activation, 3).value
      val json = Json.obj(
        "height"                  -> h,
        "finalizedHeight"         -> 1,
        "currentGenerationPeriod" -> current,
        "nextGenerationPeriod"    -> current.next
      )

      val status = json.as[FinalityStatus](using FinalityStatus.parse(Some(activation)))
      status.currentGenerationPeriod.value shouldBe current
      status.nextGenerationPeriod.value shouldBe current.next
    }
  }
}
