import org.apache.spark.{SparkConf, SparkContext}

object SparkWordCount {
  def main(args: Array[String]): Unit = {
    val conf = new SparkConf()
      .setAppName("LargeFileWordCount")
      .setMaster("yarn") 
    val sc = new SparkContext(conf)
    try {
      val inputPath = "./example.txt'
      val outputPath = "example_results'
      val wordCounts = sc.textFile(inputPath)
        .flatMap(_.toLowerCase.split("\\W+"))
        .filter(_.nonEmpty)
        .map(word => (word, 1))
        .reduceByKey(_ + _)
        .sortBy(_._2, ascending = false)
      wordCounts.saveAsTextFile(outputPath)
      wordCounts.take(20).foreach { case (word, count) =>
        println(s"$word: $count")
      }
      
    } finally {
      sc.stop()
    }
  }
}