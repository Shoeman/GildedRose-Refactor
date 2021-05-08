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
      case _ => updateDefault(item)
    }
  }

  def updateDefault(item: Item): Item = {
    if (!item.name.equals("Aged Brie")
      && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
      if (item.quality > 0) {
        item.quality = item.quality - 1
      }
    } else {
      if (item.quality < 50) {
        item.quality = item.quality + 1

        if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          if (item.sellIn < 11) {
            if (item.quality < 50) {
              item.quality = item.quality + 1
            }
          }

          if (item.sellIn < 6) {
            if (item.quality < 50) {
              item.quality = item.quality + 1
            }
          }
        }
      }
    }

    item.sellIn = item.sellIn - 1

    if (item.sellIn < 0) {
      if (!item.name.equals("Aged Brie")) {
        if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
          if (item.quality > 0) {
            item.quality = item.quality - 1
          }
        } else {
          item.quality = item.quality - item.quality
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1
        }
      }
    }
    item
  }

}