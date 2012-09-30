/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

import scala.io.Source
import java.io.File

object Comm extends App {
  
  
  override def main(args: Array[String]) {
    val first = readFile(args(0))
    val second = readFile(args(1))
    
    val output = process(first, second)
    output.foreach(println)
  }
  
  def process(first: List[String], second: List[String]): List[String] = {
    if(first.isEmpty && second.isEmpty)
      Nil
    else if(first.isEmpty)
      secondColumn(second.head) :: process(first, second.tail)
    else if(second.isEmpty)
      firstColumn(first.head) :: process(first.tail, second)
    else if(first.head == second.head)
      thirdColumn(first.head) :: process(first.tail, second.tail)
    else if(first.head > second.head)
      secondColumn(second.head) :: process(first, second.tail)
    else // first.head < second.head
      firstColumn(first.head) :: process(first.tail, second)
  }
  
  def firstColumn(str: String): String = str
  def secondColumn(str: String): String = "\t" + str
  def thirdColumn(str: String): String = "\t\t" + str

    def readFile(file: String): List[String] = {
    Source.fromFile(new File(file)).getLines.toList
  }

}