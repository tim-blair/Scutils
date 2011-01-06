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

//See tac/cat for arg parsing

val showLines = checkArg("-l")
val showBytes = checkArg("-c")
val showChars = checkArg("-m")
val showLength = checkArg("-L") || checkArg("--max-line-length")
val showWords = checkArg("-w") || checkArg("--words")

val noOpts = !(showLines || showBytes || showChars || showLength || showWords)

var totalLineCount = 0
var totalLongLine = 0
var totalCharCount = 0
var totalByteCount = 0
var totalWordCount = 0
val files = args.filter(arg => !arg.startsWith("-"))
files.foreach(arg => {
	val s = io.Source.fromFile(arg)
	val lines = s.getLines.toList
	val lineCount = lines.length
	val byteCount = s.size
	var charCount = lineCount
	var longLine = 0
	var wordCount = 0

	lines.foreach(l => {
		charCount += l.length
		longLine = longLine.max(l.length)
		wordCount += l.split("[ \t\n]+").length
	})
	
	//TODO: instead of printing here, we should store them
	// otherwise it's impossible to format them properly
	print(" ")
	if(showLines || noOpts)
		print(lineCount + " ")
	if(showWords || noOpts)
		print(wordCount + " ")
	if(showBytes)
		print(byteCount + " ")
	if(showChars || noOpts)
		print(charCount + " ")
	if(showLength)
		print(longLine + " ")
	println(arg)
	//update total vars
	totalLineCount += lineCount
	totalLongLine = totalLongLine.max(longLine)
	totalCharCount += charCount
	totalByteCount += byteCount
	totalWordCount += wordCount
})
if(files.length > 1) {
	//print total
	print(" ")
	if(showLines || noOpts)
		print(totalLineCount + " ")
	if(showWords || noOpts)
		print(totalWordCount + " ")
	if(showBytes)
		print(totalByteCount + " ")
	if(showChars || noOpts)
		print(totalCharCount + " ")
	if(showLength)
		print(totalLongLine + " ")
	println("total")
}

//print total
def checkArg(str: String) =
	args.length > 0 && args.contains(str)

def getArg(str: String, default: String): String = {
	var value = default
	var prev = ""
	if(checkArg(str)) {
		args.foreach(arg => {
			if(prev == str)
				value = arg
			prev = arg
		})
	}
	value
}
