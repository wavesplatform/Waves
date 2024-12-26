package com.wavesplatform.network

import com.wavesplatform.common.utils.Base64
import com.wavesplatform.test._

class MicroBlockResponseSpec extends FreeSpec {
  private val microBlockBytes = Base64.decode(
    "CqMCCAUSINDAl9MzGkepj6WsZ+1NZv0grSgzJogVswMTP7+ug6LoGkDJBEdWcsDc/bat2ljrW74o9l7Y+Pcp07ra2VRe/HLU/Oq6cT7xJqyqZ7xoXP5tLKcnq4hF/5FtS/NYx5zX3RMPIiDk9FCvGrDyyqy8jzX1qe6cEdv5NRXv+hSH+BMnnFtqBSqYAQpUCFQSIBjkoQIwpcrsWlpsgLJVOBo27loBDODD+h473uYYaxMSGgQQoI0GIOvZ5ffzLigDwgYeChYKFCCTgv+auCSYevJXZ7mkKyv2/dkoEgQQoI0GEkD5K5E+HKr3IXYhnwLZaWVsIF+tJdbvV4LFjksWIeLoopDf46TTE2XXXb64R2ZsbWV0QJpQ3cNqTnKXGcB2DesIEkA+/2wKSB07Tg2uBH9OGuIXLBH7FzKPLllyjn7TlvYTLZrohyNSBAIQ3sM9UwPQkUDSC1NGYBFwRHRdF+gPfQcDGiAzlpLCohmCR1KXVnxw5AVO7Xq60gorXfInMXSiS3Qf9Q=="
  )

  "Signature is valid" in {
    val parsedMicroBlock = PBMicroBlockSpec.deserializeData(microBlockBytes).get.microblock
    parsedMicroBlock.signatureValid() shouldBe true
  }
}
