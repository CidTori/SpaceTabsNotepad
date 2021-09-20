import org.scalatest.FlatSpec

import elasticTabstops.{calcElasticTabstopPositions, spacesToSpaceTabs, spaceTabsToSpaces}


class ElasticTabstopsSpec extends FlatSpec {

  "spaceTabsToSpaces" should "replace space tabs with spaces correctly" in {
    assert(spaceTabsToSpaces("\ty", 4) == "    y")
    assert(spaceTabsToSpaces("x \ty", 4) == "x   y")
    assert(spaceTabsToSpaces("xxxxxxx \ty", 4) == "xxxxxxx  y")

    val given = List(
      " \ty",
      "xxxxxxx \ty"
    ).mkString("\n")

    val expected = List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")

    assert(spaceTabsToSpaces(given, 4) == expected)

    val given2 = List(
      " \t",
      "xxxxxxx"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxxxxxx"
    ).mkString("\n")

    assert(spaceTabsToSpaces(given2, 4) == expected2)
  }

  "spacesToTabs" should "replace spaces with tabs correctly" in {
    assert(spacesToSpaceTabs("    y", 4) == "\ty")
    assert(spacesToSpaceTabs("x   y", 4) == "x \ty")
    assert(spacesToSpaceTabs("xxxxxxx  y", 4) == "xxxxxxx \ty")

    val given = List(
      "         y",
      "xxxxxxx  y"
    ).mkString("\n")

    val expected = List(
      " \ty",
      "xxxxxxx \ty"
    ).mkString("\n")

    assert(spacesToSpaceTabs(given, 4) == expected)

    val given2 = List(
      "       ",
      "xxx"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxx"
    ).mkString("\n")

    assert(spacesToSpaceTabs(given2, 4) == expected2)
  }

}
