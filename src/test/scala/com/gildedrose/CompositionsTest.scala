package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompositionsTest extends AnyFlatSpec with Matchers{

  "orFun" should "compose a PartialFunction and Function in an orElse manner" in {
    import com.gildedrose.Compositions.PartialFunToFun

    val partial: PartialFunction[(Int, Int), String] = {
      case (0, 0) => "partial"
    }
    val fun: (Int, Int) => String = (a, b) => "fun"

    val composed = partial orFun fun

    composed(0, 0) should equal("partial")
    composed(0, 1) should equal("fun")
  }

  "andThen" should "join the output of a Function2 with the input of a Function" in {
    import com.gildedrose.Compositions.TwoToOneFun

    val fun2: (Int, Int) => Int = _ + _
    val fun: Int => Int = _ + 1

    val composed = fun2 andThen fun

    composed(2, 2) should equal(5)
  }
}
