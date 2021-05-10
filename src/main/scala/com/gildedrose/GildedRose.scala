package com.gildedrose

import com.gildedrose.GildedRose.updateItem


class GildedRose(val items: Array[Item]) {

  def updateQuality() {
    for (i <- items.indices) {
      items(i) = updateItem(items(i))
    }
  }
}

object GildedRose {

  // Unfortunately Item isn't a case class so we can't extract values from it via pattern matching
  def extractItem(item: Item): (String, Int, Int) = (item.name, item.sellIn, item.quality)

  def updateItem(item: Item): Item = {

    item.name match {
      case "Sulfuras, Hand of Ragnaros" => item
      case _ => advanceItem(item)
    }
  }

  def advanceItem(item: Item): Item = {

    val (name, sellIn, quality) = extractItem(item)

    var newQuality = name match {
      case "Aged Brie" => brieQuality(sellIn, quality)
      case "Backstage passes to a TAFKAL80ETC concert" => backstageQuality(sellIn, quality)
      case _ => adjustDefaultQuality(sellIn, quality)
    }

    if (newQuality < 0) {
      newQuality = 0
    }
    new Item(name, sellIn - 1, newQuality)
  }

  def brieQuality(sellIn: Int, quality: Int): Int = {
    // A quirk from the original behaviour is that is doesn't cap to 50 if it's already over
    if (quality >= 50) {
      return quality
    }

    val newQuality = adjustBrieQuality(sellIn, quality)

    capQuality(newQuality)
  }

  val capQuality: Int => Int = Math.min(50, _)

  val preserveQuality: PartialFunction[(Int, Int), Int] = {
    case (sellIn, quality) if quality >= 50 => quality
  }
  val expireQuality: PartialFunction[(Int, Int), Int] = {
    case (sellIn, quality) if sellIn <= 0 => 0
  }

  val adjustBackstageQuality: PartialFunction[(Int, Int), Int] = {
    case (sellIn, quality) =>
      val increase = sellIn match {
        case x if x <= 5 => 3
        case x if x <= 10 => 2
        case _ => 1
      }
      quality + increase
  }

  val backstageQuality: PartialFunction[(Int, Int), Int] = expireQuality orElse preserveQuality orElse (adjustBackstageQuality andThen capQuality)
  val adjustBrieQuality: (Int, Int) => Int = adjustQuality(-1, -2)
  val adjustDefaultQuality: (Int, Int) => Int = adjustQuality(1, 2)

  def adjustQuality(normal: Int, overdue: Int)(sellIn: Int, quality: Int): Int = {
    val decrease = if (sellIn > 0) normal else overdue
    quality - decrease
  }
}