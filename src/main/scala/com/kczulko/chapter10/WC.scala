package com.kczulko.chapter10

sealed trait WC {

}

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC
