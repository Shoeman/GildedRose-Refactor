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

  import com.gildedrose.Compositions._

  def updateItem(item: Item): Item = {
    item.name match {
      case "Sulfuras, Hand of Ragnaros" => item
      case _ => advanceItem(item)
    }
  }

  def advanceItem(item: Item): Item = {

    // Unfortunately Item isn't a case class so we can't extract values from it via pattern matching
    val (name, sellIn, quality) = (item.name, item.sellIn, item.quality)

    val newQuality = name match {
      case "Aged Brie" => brieQuality(sellIn, quality)
      case "Backstage passes to a TAFKAL80ETC concert" => backstageQuality(sellIn, quality)
      case _ => defaultQuality(sellIn, quality)
    }
    new Item(name, sellIn - 1, newQuality)
  }

  // Quirk from the original behaviour where quality doesn't update if it's already over 50
  val preserveQuality: PartialFunction[(Int, Int), Int] = {
    case (_, quality) if quality >= 50 => quality
  }
  val expireQuality: PartialFunction[(Int, Int), Int] = {
    case (sellIn, _) if sellIn <= 0 => 0
  }

  def adjustQuality(normal: Int, overdue: Int)(sellIn: Int, quality: Int): Int = {
    val decrease = if (sellIn > 0) normal else overdue
    quality - decrease
  }

  val adjustDefaultQuality: (Int, Int) => Int = adjustQuality(1, 2)
  val adjustBrieQuality: (Int, Int) => Int = adjustQuality(-1, -2)
  val adjustBackstageQuality: (Int, Int) => Int = (sellIn: Int, quality: Int) => {
    val increase = sellIn match {
      case x if x <= 5 => 3
      case x if x <= 10 => 2
      case _ => 1
    }
    quality + increase
  }

  val capQuality: Int => Int = Math.min(50, _)
  val floorQuality: Int => Int = Math.max(0, _)

  // Compositions
  val defaultQuality: (Int, Int) => Int = adjustDefaultQuality andThen floorQuality
  val brieQuality: (Int, Int) => Int = preserveQuality orFun (adjustBrieQuality andThen capQuality)
  val backstageQuality: (Int, Int) => Int = expireQuality orElse preserveQuality orFun (adjustBackstageQuality andThen capQuality)
}