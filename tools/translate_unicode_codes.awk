match($0, /^_(.+)_(.+)$/, G) {
	for (i = strtonum("0x" G[1]); i <= strtonum("0x" G[2]); i++)
		printf("#x%X ", i);
	next
	print ""
}
match($0, /^_(.+)$/, G) {
	print "#x" G[1]
	next
}
