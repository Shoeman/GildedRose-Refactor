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

  val capQuality: Int => Int = Math.min(50, _)
  val floorQuality: Int => Int = Math.max(0, _)

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

  val adjustBrieQuality: (Int, Int) => Int = adjustQuality(-1, -2)
  val adjustDefaultQuality: (Int, Int) => Int = adjustQuality(1, 2)

  val partialBrie: PartialFunction[(Int, Int), Int] = {
    case (sellIn, quality) => adjustBrieQuality(sellIn, quality)
  }

  val partialDefault: PartialFunction[(Int, Int), Int] = {
    case (sellIn, quality) => adjustDefaultQuality(sellIn, quality)
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

  // Compositions
  val brieQuality: PartialFunction[(Int, Int), Int] = preserveQuality orElse (partialBrie andThen capQuality)
  val backstageQuality: PartialFunction[(Int, Int), Int] = expireQuality orElse preserveQuality orElse (adjustBackstageQuality andThen capQuality)
  val defaultQuality: PartialFunction[(Int, Int), Int] = partialDefault andThen floorQuality
}