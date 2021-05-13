package com.gildedrose

object Compositions {

  /**
   * Equivalent of orElse but taking and returning a Function
   *
   * @param par PartialFunction
   * @tparam A input type
   * @tparam B output type
   */
  implicit class PartialFunToFun[A, B](val par: PartialFunction[(A, A), B]) extends AnyVal {
    def orFun(fun: (A, A) => B): (A, A) => B = {
      (inp1: A, inp2: A) => if (par.isDefinedAt((inp1, inp2))) par.apply(inp1, inp2) else fun(inp1, inp2)
    }
  }

  /**
   * 'andThen' composition to join the output of a Function2 and the input of a Function
   *
   * @param fun2 Function2
   * @tparam A the input and output type
   */
  implicit class TwoToOneFun[A](val fun2: (A, A) => A) extends AnyVal {
    def andThen(fun: A => A): (A, A) => A = {
      (inp1: A, inp2: A) => fun(fun2(inp1, inp2))
    }
  }
}