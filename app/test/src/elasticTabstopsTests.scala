import org.scalatest.FlatSpec

import elasticTabstops.{calcElasticTabstopPositions, smartTabsToSpaceTabs, spaceTabsToSmartTabs}


class ElasticTabstopsSpec extends FlatSpec {

  "spaceTabsToSmartTabs" should "replace space tabs with smart tabs correctly" in {
    assert(spaceTabsToSmartTabs(" \t\ty", 4) == "    \ty")
    assert(spaceTabsToSmartTabs("x \t\ty", 4) == "x   \ty")
    assert(spaceTabsToSmartTabs("xxxxxxx \t\ty", 4) == "xxxxxxx  \ty")

    val given = List(
      " \t\ty",
      "xxxxxxx \t\ty"
    ).mkString("\n")

    val expected = List(
      "         \ty",
      "xxxxxxx  \ty"
    ).mkString("\n")

    assert(spaceTabsToSmartTabs(given, 4) == expected)

    val given2 = List(
      " \t",
      "xxxxxxx\t"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxxxxxx\t"
    ).mkString("\n")

    assert(spaceTabsToSmartTabs(given2, 4) == expected2)
  }

  "spacesToTabs" should "replace spaces with tabs correctly" in {
    assert(smartTabsToSpaceTabs("    \ty", 4) == " \t\ty")
    assert(smartTabsToSpaceTabs("x   \ty", 4) == "x \t\ty")
    assert(smartTabsToSpaceTabs("xxxxxxx  \ty", 4) == "xxxxxxx \t\ty")

    val given = List(
      "         \ty",
      "xxxxxxx  \ty"
    ).mkString("\n")

    val expected = List(
      " \t\ty",
      "xxxxxxx \t\ty"
    ).mkString("\n")

    assert(smartTabsToSpaceTabs(given, 4) == expected)

    val given2 = List(
      "       ",
      "xxx\t"
    ).mkString("\n")

    val expected2 = List(
      "",
      "xxx\t"
    ).mkString("\n")

    assert(smartTabsToSpaceTabs(given2, 4) == expected2)
  }

}
