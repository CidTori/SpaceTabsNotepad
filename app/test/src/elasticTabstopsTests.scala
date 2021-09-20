import org.scalatest.FlatSpec

import elasticTabstops.{calcElasticTabstopPositions, spacesToSpaceTabs, spaceTabsToSpaces}


class ElasticTabstopsSpec extends FlatSpec {

  "spaceTabsToSpaces" should "replace space tabs with spaces correctly" in {
    assert(spaceTabsToSpaces(" \t\ty", 4) == "    \ty")
    assert(spaceTabsToSpaces("x \t\ty", 4) == "x   \ty")
    assert(spaceTabsToSpaces("xxxxxxx \t\ty", 4) == "xxxxxxx  \ty")

    val given = List(
      " \t\ty",
      "xxxxxxx \t\ty"
    ).mkString("\n")

    val expected = List(
      "         \ty",
      "xxxxxxx  \ty"
    ).mkString("\n")

    assert(spaceTabsToSpaces(given, 4) == expected)

    val given2 = List(
      " \t",
      "xxxxxxx\t"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxxxxxx\t"
    ).mkString("\n")

    assert(spaceTabsToSpaces(given2, 4) == expected2)
  }

  "spacesToTabs" should "replace spaces with tabs correctly" in {
    assert(spacesToSpaceTabs("    \ty", 4) == " \t\ty")
    assert(spacesToSpaceTabs("x   \ty", 4) == "x \t\ty")
    assert(spacesToSpaceTabs("xxxxxxx  \ty", 4) == "xxxxxxx \t\ty")

    val given = List(
      "         \ty",
      "xxxxxxx  \ty"
    ).mkString("\n")

    val expected = List(
      " \t\ty",
      "xxxxxxx \t\ty"
    ).mkString("\n")

    assert(spacesToSpaceTabs(given, 4) == expected)

    val given2 = List(
      "       ",
      "xxx\t"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxx\t"
    ).mkString("\n")

    assert(spacesToSpaceTabs(given2, 4) == expected2)
  }

}
