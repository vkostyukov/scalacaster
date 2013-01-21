package scalacaster.sort

class MergeSorter extends Sorter {

  def sort(input: Array[Int]): Array[Int] = {
    if (input.length == 1) {
      return Array(input(0))
    } else {
      val n = input.length / 2

      val left = sort(input.slice(0, n))
      val right = sort(input.slice(n, input.length))

      var result: Array[Int] = new Array(input.length)

      var i = 0
      var j = 0
      result.indices foreach { k =>
        if (i == left.length) {
          result(k) = right(j)
          j += 1
        } else if (j == right.length) {
          result(k) = left(i)
          i += 1
        } else if (left(i) > right(j)) {
          result(k) = right(j)
          j += 1
        } else {
          result(k) = left(i)
          i += 1
        }
      }

      return result
    }
  }
}