package com.htmlism.csvpercentage

object Analyzer {
  def analyze(xs: List[Row]): Unit = {
    val length = getNumIterations(xs)

    for (n <- 1 to length) {
      println
      println
      println(s"starting $n of $length")

      val tightTotals = collection.mutable.Map[(String, String), Int]().withDefault(_ => 0)
      val looseTotals = collection.mutable.Map[String, Int]().withDefault(_ => 0)

      // build sums over the lesser dimensions
      for (row <- xs) {
        val tightKey = row.slice(0, n).mkString("\t")
        val looseKey = row.slice(0, n - 1).mkString("+")

        tightTotals((tightKey, looseKey)) += row.last.toInt
        looseTotals(looseKey) += row.last.toInt
      }

      // now print as percentages of the total
      for (((tight, loose), v) <- tightTotals.toList.sorted) {
        val pct = "%.2f".format(tightTotals(tight, loose).toDouble / looseTotals(loose) * 100)

        val enc = Seq(tight, loose, pct).mkString("\t")

        println(enc)
      }
    }
  }

  def getNumIterations(xs: List[Row]): Int = {
    val x = xs.head

    // last column is count by the aggregate fields on the left
    x.length - 1
  }
}
