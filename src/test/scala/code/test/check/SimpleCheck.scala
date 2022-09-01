package code.test.check

import org.scalacheck.Prop.forAll
import org.scalacheck.Test.{Parameters, TestCallback}
import org.scalacheck.{Properties, Test}

object SimpleCheck extends App {

  case class Simple(str: String) {
    def length: Int = str.length
  }

  object PropertiesUnitCheck extends Properties("checkSimpleLength") {

    override def overrideParameters(p: Parameters): Parameters = {
      p
        .withMinSuccessfulTests(50)
        .withTestCallback(new TestCallback {
          override def onPropEval(name: String, threadIdx: Int, succeeded: Int, discarded: Int): Unit = {
            println(s"Evaluating prop with name: $name")
          }
        })
    }

    property("length") = forAll { (str: String) =>
      Simple(str).length >= 0
    }
  }

}
