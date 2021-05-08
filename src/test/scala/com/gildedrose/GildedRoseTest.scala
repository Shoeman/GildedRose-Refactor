package com.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest extends AnyFlatSpec with Matchers {

  def extractItem(item: Item): (String, Int, Int) = (item.name, item.sellIn, item.quality)

  it should "foo" in {
    val items = Array[Item](new Item("foo", 0, 0))
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0).name should equal ("foo")
  }

  "Empty input" should "not break" in {
    val items = Array[Item]()
    val app = new GildedRose(items)
    app.updateQuality()
    app.items.length should equal (0)
  }

  // Coverage tests before refactoring
  "Normal Items" should "update as expected" in {
    val items = Array[Item](
      new Item("foo", 1, 10),
      new Item("foo", 0, 10)
    )
    val app = new GildedRose(items)
    app.updateQuality()
    extractItem(app.items(0)) should equal ("foo", 0, 9)
    extractItem(app.items(1)) should equal ("foo", -1, 8)
  }

  "Brie Items" should "update as expected" in {
    val items = Array[Item](
      new Item("Aged Brie", 1, 10),
      new Item("Aged Brie", 0, 10),
      new Item("Aged Brie", 1, 50),
      new Item("Aged Brie", 0, 50),
      new Item("Aged Brie", 1, 0),
      new Item("Aged Brie", 1, 51)
    )
    val app = new GildedRose(items)
    app.updateQuality()
    extractItem(app.items(0)) should equal ("Aged Brie", 0, 11)
    extractItem(app.items(1)) should equal ("Aged Brie", -1, 12)
    extractItem(app.items(2)) should equal ("Aged Brie", 0, 50)
    extractItem(app.items(3)) should equal ("Aged Brie", -1, 50)
    extractItem(app.items(4)) should equal ("Aged Brie", 0, 1)
    extractItem(app.items(5)) should equal ("Aged Brie", 0, 51)
  }

  "Sulfuras Items" should "update as expected" in {
    val items = Array[Item](
      new Item("Sulfuras, Hand of Ragnaros", 1, 80),
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
      new Item("Sulfuras, Hand of Ragnaros", 0, 40),
      new Item("Sulfuras, Hand of Ragnaros", -1, 80)
    )
    val app = new GildedRose(items)
    app.updateQuality()
    extractItem(app.items(0)) should equal ("Sulfuras, Hand of Ragnaros", 1, 80)
    extractItem(app.items(1)) should equal ("Sulfuras, Hand of Ragnaros", 0, 80)
    extractItem(app.items(2)) should equal ("Sulfuras, Hand of Ragnaros", 0, 40)
    extractItem(app.items(3)) should equal ("Sulfuras, Hand of Ragnaros", -1, 80)
  }

  "Backstage Items" should "update as expected" in {
    val items = Array[Item](
      new Item("Backstage passes to a TAFKAL80ETC concert", 11, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 6, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 1, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 0, 50),
      new Item("Backstage passes to a TAFKAL80ETC concert", 1, 50),
      new Item("Backstage passes to a TAFKAL80ETC concert", 1, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 6, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 11, 49)
    )
    val app = new GildedRose(items)
    app.updateQuality()
    extractItem(app.items(0)) should equal ("Backstage passes to a TAFKAL80ETC concert", 10, 11)
    extractItem(app.items(1)) should equal ("Backstage passes to a TAFKAL80ETC concert", 9, 12)
    extractItem(app.items(2)) should equal ("Backstage passes to a TAFKAL80ETC concert", 5, 12)
    extractItem(app.items(3)) should equal ("Backstage passes to a TAFKAL80ETC concert", 4, 13)
    extractItem(app.items(4)) should equal ("Backstage passes to a TAFKAL80ETC concert", 0, 13)
    extractItem(app.items(5)) should equal ("Backstage passes to a TAFKAL80ETC concert", -1, 0)
    extractItem(app.items(6)) should equal ("Backstage passes to a TAFKAL80ETC concert", -1, 0)
    extractItem(app.items(7)) should equal ("Backstage passes to a TAFKAL80ETC concert", 0, 50)
    extractItem(app.items(8)) should equal ("Backstage passes to a TAFKAL80ETC concert", 0, 50)
    extractItem(app.items(9)) should equal ("Backstage passes to a TAFKAL80ETC concert", 5, 50)
    extractItem(app.items(10)) should equal ("Backstage passes to a TAFKAL80ETC concert", 10, 50)
  }
}