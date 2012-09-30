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
    val fileNames = args.takeRight(2)
    val arguments = args.dropRight(2)
    val first = readFile(fileNames(0))
    val second = readFile(fileNames(1))
    
    val output = process(first, second, arguments)
    output.foreach(str => str match {
      case None =>
      case Some(string) => println(string)
    })
  }
  
  def process(first: List[String], second: List[String], args: Array[String]): List[Option[String]] = {
    def firstColumn(str: String): Option[String] = if(args.contains("-1")) None else Some(str)
    def secondColumn(str: String): Option[String] = if(args.contains("-2")) None else Some("\t" + str)
    def thirdColumn(str: String): Option[String] = if(args.contains("-3")) None else Some("\t\t" + str)

	if(first.isEmpty && second.isEmpty)
      Nil
    else if(first.isEmpty)
      secondColumn(second.head) :: process(first, second.tail, args)
    else if(second.isEmpty)
      firstColumn(first.head) :: process(first.tail, second, args)
    else if(first.head == second.head)
      thirdColumn(first.head) :: process(first.tail, second.tail, args)
    else if(first.head > second.head)
      secondColumn(second.head) :: process(first, second.tail, args)
    else // first.head < second.head
      firstColumn(first.head) :: process(first.tail, second, args)
  }
  

    def readFile(file: String): List[String] = {
    Source.fromFile(new File(file)).getLines.toList
  }

}