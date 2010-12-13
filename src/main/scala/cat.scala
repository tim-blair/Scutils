//-n means number each line
val numbered = checkArg("-n")
//-b means number non-empty lines, overrides -n
val nonBlankNum = checkArg("-b")
//-E display $ at end of each line
val showEnds = checkArg("-E")
//-s suppress repeated empty lines
val suppress = checkArg("-s")

val src = 
	if(args.length > 0 && args.contains("-"))
		io.Source.fromFile(args(0))
	else
		io.Source.fromInputStream(System.in)
var num = 1
var prevWasBlank = false
src.getLines.foreach(line => {
	if(suppress && prevWasBlank && line == "")
		continue; //except not...
	prevWasBlank = line == ""
	if(nonBlankNum) {
		//Re-use the check we already did
		if(prevWasBlank) {
			printf("%6d ", num)
			num += 1
		}
	} else if(numbered) {
		//real cat pads and puts a space -- assumes under 1 million lines
		printf("%6d ", num)
		num += 1
	}
	print(line)
	if(showEnds)
		print("$")
	println()
})

def checkArg(str: String) = {
	args.length > 0 && args.contains(str)
}
