/**
 * This file is a part of Scalacaster project.
 * 
 * Author: Vladimir Kostyukov (vkostyukov.ru)
 * Project Page: https://github.com/vkostyukov/scalacaster
 * License: Apache 2.0.
 */
package scalacaster.sort

trait Sorter {
  def sort(input: Array[Int]): Array[Int]
}

