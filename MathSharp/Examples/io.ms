copyFile() = writeFile("out.txt", readFile("in.txt"))

echoInt() = println(readInt())

main() = copyFile()