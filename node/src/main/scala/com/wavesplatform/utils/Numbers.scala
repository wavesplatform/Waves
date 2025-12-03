package com.wavesplatform.utils

object Numbers {
  @inline def when(cond: Boolean)(x: Long): Long = if (cond) x else 0L
}
