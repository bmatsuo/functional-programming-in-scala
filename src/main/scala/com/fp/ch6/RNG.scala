package com.fp.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed*0x5DEECE66DL +  0xBL) & ((1L << 48) - 1)
      ((seed >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }
}
