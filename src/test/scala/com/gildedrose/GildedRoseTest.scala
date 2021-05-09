package com.gildedrose

import com.gildedrose.matchers.GildedRoseMatchers
import org.scalatest.Inspectors.forAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseTest extends AnyFlatSpec with Matchers with GildedRoseMatchers {

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
    app.items shouldBe empty
  }

  "updateQuality" should "handle multple items" in {
    val items = Array[Item](
      new Item("Sulfuras, Hand of Ragnaros", 1, 80),
      new Item("Aged Brie", 0, 10),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10),
      new Item("foo", 1, 10),
    )
    val app = new GildedRose(items)
    app.updateQuality()
    app.items(0) should matchItemValues("Sulfuras, Hand of Ragnaros", 1, 80)
    app.items(1) should matchItemValues("Aged Brie", -1, 12)
    app.items(2) should matchItemValues("Backstage passes to a TAFKAL80ETC concert", 4, 13)
    app.items(3) should matchItemValues("foo", 0, 9)
  }

  // More granular tests with the individual updateItem method
  "Sulfuras items" should "never change" in {

    val tests = List(
      (new Item("Sulfuras, Hand of Ragnaros", 1, 80), ("Sulfuras, Hand of Ragnaros", 1, 80)),
      (new Item("Sulfuras, Hand of Ragnaros", 0, 80), ("Sulfuras, Hand of Ragnaros", 0, 80)),
      (new Item("Sulfuras, Hand of Ragnaros", 0, 40), ("Sulfuras, Hand of Ragnaros", 0, 40)),
      (new Item("Sulfuras, Hand of Ragnaros", -1, 80), ("Sulfuras, Hand of Ragnaros", -1, 80))
    )

    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  "Aged Brie Items" should "update as expected" in {
    val tests = List(
      (new Item("Aged Brie", 1, 10), ("Aged Brie", 0, 11)),
      (new Item("Aged Brie", 0, 10), ("Aged Brie", -1, 12)),
      (new Item("Aged Brie", 1, 0), ("Aged Brie", 0, 1)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  it should "not exceed 50 quality" in {
    val tests = List(
      (new Item("Aged Brie", 1, 50), ("Aged Brie", 0, 50)),
      (new Item("Aged Brie", 0, 50), ("Aged Brie", -1, 50)),
      (new Item("Aged Brie", 0, 49), ("Aged Brie", -1, 50)),
      // This case is from the original behaviour, it seems like a bug but the requirements are vague
      (new Item("Aged Brie", 1, 51), ("Aged Brie", 0, 51))
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  "Backstage Items" should "update as expected" in {
    val tests = List(
      (new Item("Backstage passes to a TAFKAL80ETC concert", 11, 10), ("Backstage passes to a TAFKAL80ETC concert", 10, 11)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 10, 10), ("Backstage passes to a TAFKAL80ETC concert", 9, 12)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 6, 10), ("Backstage passes to a TAFKAL80ETC concert", 5, 12)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 5, 10), ("Backstage passes to a TAFKAL80ETC concert", 4, 13)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 1, 10), ("Backstage passes to a TAFKAL80ETC concert", 0, 13)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  it should "lose all value when expired" in {
    val tests = List(
      (new Item("Backstage passes to a TAFKAL80ETC concert", 0, 10), ("Backstage passes to a TAFKAL80ETC concert", -1, 0)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 0, 50), ("Backstage passes to a TAFKAL80ETC concert", -1, 0)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 0, 51), ("Backstage passes to a TAFKAL80ETC concert", -1, 0)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  it should "not exceed 50 quality" in {
    val tests = List(
      (new Item("Backstage passes to a TAFKAL80ETC concert", 1, 50), ("Backstage passes to a TAFKAL80ETC concert", 0, 50)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 1, 49), ("Backstage passes to a TAFKAL80ETC concert", 0, 50)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 6, 49), ("Backstage passes to a TAFKAL80ETC concert", 5, 50)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 11, 49), ("Backstage passes to a TAFKAL80ETC concert", 10, 50)),

      // Quirk from the original behaviour when the quality is already above 50
      (new Item("Backstage passes to a TAFKAL80ETC concert", 1, 51), ("Backstage passes to a TAFKAL80ETC concert", 0, 51)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 6, 51), ("Backstage passes to a TAFKAL80ETC concert", 5, 51)),
      (new Item("Backstage passes to a TAFKAL80ETC concert", 11, 51), ("Backstage passes to a TAFKAL80ETC concert", 10, 51)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  "Default Items" should "update as expected" in {
    val tests = List(
      (new Item("foo", 1, 10), ("foo", 0, 9)),
      (new Item("foo", 0, 10), ("foo", -1, 8)),
      (new Item("foo", 1, 51), ("foo", 0, 50)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }

  it should "not drop quality below 0" in {
    val tests = List(
      (new Item("foo", 1, 0), ("foo", 0, 0)),
      (new Item("foo", 0, 1), ("foo", -1, 0)),
    )
    forAll(tests) {
      case (item, expected) => GildedRose.updateItem(item) should matchItemValues(expected)
    }
  }
}