cd test/files
echo * | tr " " "\n" | sort
echo */* | tr " " "\n" | sort
echo */*/*.mp3 | tr " " "\n" | sort
echo */*/smelly*.mp3 | tr " " "\n" | sort
echo monty_*/spam_*.txt | tr " " "\n" | sort
echo fri?nds | tr " " "\n" | sort
echo friend?/* | tr " " "\n" | sort
echo friends/* | tr " " "\n" | sort
