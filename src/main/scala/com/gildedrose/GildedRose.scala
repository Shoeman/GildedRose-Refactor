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

    val itemValues = extractItem(item)

    itemValues match {
      case ("Sulfuras, Hand of Ragnaros", _, _) => item
      case ("Aged Brie", sellIn, quality) => updateBrie(sellIn, quality)
      case ("Backstage passes to a TAFKAL80ETC concert", sellIn, quality) => updateBackstage(sellIn, quality)
      case (name, sellIn, quality) => advanceItem(name, sellIn, quality)
    }
  }

  def advanceItem(name: String, sellIn: Int, quality: Int): Item = {

    var newQuality = name match {
      case _ => defaultQuality(sellIn, quality)
    }

    if (newQuality < 0) {
      newQuality = 0
    }
    new Item(name, sellIn - 1, newQuality)
  }

  def defaultQuality(sellIn: Int, quality: Int): Int = {
    val decrease = if (sellIn > 0) 1 else 2
    quality - decrease
  }

  def updateBrie(sellIn: Int, quality: Int): Item = {

    // A quirk from the original behaviour is that is doesn't cap to 50 if it's already over
    if (quality >= 50) {
      return new Item("Aged Brie", sellIn - 1, quality)
    }

    val increase = if (sellIn > 0) 1 else 2
    var newQuality = quality + increase

    if (newQuality > 50) {
      newQuality = 50
    }
    new Item("Aged Brie", sellIn - 1, newQuality)
  }

  def updateBackstage(sellIn: Int, quality: Int): Item = {

    if (sellIn <= 0) {
      return new Item("Backstage passes to a TAFKAL80ETC concert", sellIn - 1, 0)
    }

    // A quirk from the original behaviour is that is doesn't cap to 50 if it's already over
    if (quality >= 50) {
      return new Item("Backstage passes to a TAFKAL80ETC concert", sellIn - 1, quality)
    }

    val increase = sellIn match {
      case x if x <= 5 => 3
      case x if x <= 10 => 2
      case _ => 1
    }
    var newQuality = quality + increase

    if (newQuality > 50) {
      newQuality = 50
    }
    new Item("Backstage passes to a TAFKAL80ETC concert", sellIn - 1, newQuality)
  }
}