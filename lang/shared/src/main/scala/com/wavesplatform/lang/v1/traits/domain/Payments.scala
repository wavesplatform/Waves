package com.wavesplatform.lang.v1.traits.domain

import com.wavesplatform.common.state.ByteStr

sealed trait Payments

object Payments {
  type Payment = (Long, Option[ByteStr])

  case class Single(p: Option[Payment]) extends Payments
  case class Multi(p: Seq[Payment])     extends Payments
}