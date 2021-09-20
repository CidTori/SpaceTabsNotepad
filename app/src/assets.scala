package object assets {
  val InitialText = StringContext.treatEscapes(
    """
      |
      | \t--------| \t-------------------------- \t|--------
      | \t--------| \tWelcome to Elastic Notepad \t|--------
      | \t--------| \t-------------------------- \t|--------
      | \t--------| \t \t|--------
      | \t--------| \tThis is your scratch file. \t|--------
      | \t--------| \tIt's initialised with this \t|--------
      | \t--------| \ttext so you can play with \t|--------
      | \t--------| \telastic tabstops. \t|--------
      | \t--------| \t \t|--------
      | \t--------| \t-------------------------- \t|--------
      |
      |   \t/* try making \t*/
      |int someDemoCode( \tint start,   \t/* try making \t*/
      | \tint length)   \t/* try making \t*/
      |{   \t/* try making \t*/
      |\tx() \t//x()   \t/* try making \t*/
      |\tprint(\"hello!\") \t//print(\"hello!\")   \t/* this comment \t*/
      |\tdoSomethingComplicated() \t//doSomethingComplicated()   \t/* a bit longer \t*/
      |\tfor (i in range(start, length)) \t//for (i in range(start, length))   \t/* a bit longer \t*/
      |\t{ \t//{   \t/* a bit longer \t*/
      |\t\tif (isValid(i)) \t//\tif (isValid(i))   \t/* a bit longer \t*/
      |\t\t{ \t//\t{   \t/* a bit longer \t*/
      |\t\t\tcount++ \t//\t\tcount++   \t/* a bit longer \t*/
      |\t\t} \t//\t}   \t/* a bit longer \t*/
      |\t} \t//}   \t/* a bit longer \t*/
      |\treturn count \t//return count   \t/* a bit longer \t*/
      |} \t   \t/* a bit longer \t*/
      | \t   \t/* a bit longer \t*/
      |
      |You can use elastic tabstops with tables and TSV files too
      |
      |Title\tAuthor\tPublisher\tYear
      |Generation X\tDouglas Coupland\tAbacus\t1995
      |Informagic\tJean-Pierre Petit\tJohn Murray Ltd\t1982
      |The Cyberiad\tStanislaw Lem\tHarcourt Publishers Ltd\t1985
      |The Selfish Gene\tRichard Dawkins\tOxford University Press\t2006""".stripMargin
  )
}
