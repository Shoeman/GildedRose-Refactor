package com.gildedrose.matchers

import com.gildedrose.Item
import org.scalatest.matchers.{MatchResult, Matcher}

trait GildedRoseMatchers {

  class ItemValuesMatcher(right: (String, Int, Int)) extends Matcher[Item] {

    override def apply(left: Item): MatchResult = {
      val leftValues = (left.name, left.sellIn, left.quality)
      val matches = leftValues == right

      val failureMessage = s"The item values $leftValues did not equal $right"
      val negatedMessage = s"The item values $leftValues did equal $right"

      MatchResult(matches, failureMessage, negatedMessage)
    }
  }
  def matchItemValues(right: (String, Int, Int)): Matcher[Item] = new ItemValuesMatcher(right)
}
