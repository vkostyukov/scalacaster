package scalacaster.sort

object SorterBenchmark extends App {

  val sorter = new MergeSorter
  sorter.sort(Array(1,5,7,10,2,11,14,3,7,9)) foreach { x => print(x + " ") }

}