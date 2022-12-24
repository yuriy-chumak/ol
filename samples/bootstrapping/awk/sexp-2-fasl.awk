/^\($/ { next } # skip leading (
/^\)$/ { next }

BEGIN {
	# char -> integer table
	for (n=1; n<32768; n++)
		ord[sprintf("%c",n)] = n

	# ol constants in form
	# "not-a-reference type value"
	NULL =   "0 13 2" # 13 is a type-constant
	EMPTY =  "0 13 3"
	EOF =    "0 13 4"

	TRUE =   "0 13 1"
	FALSE =  "0 13 0"

	STDIN =  "0 12 0" # 12 is a type-port
	STDOUT = "0 12 1"
	STDERR = "0 12 2"

	print "" # debug newline
}

# the string is ansi-string?
function ansiq(arr, n)
{
	for (i = 1; i <= n; i++)
		if (ord[arr[i]] > 255)
			return 0
	return 1
}

function number(num,     s,i,p)
{
	num = num < 0 ? -num : num
	for(p = 0; ; p++) {
		if (num < 128) {
			# for (i = --p; i >= 0; i--)
			# 	s = arr[i] " " s " "
			for (i = 0; i < p; i++)
				s = s arr[i] " "
			s = s num
			return s;
		}
		else {
			arr[p] = or(and(num, 127), 128)
			num = rshift(num, 7)
		}
	}
}

match($0, /^\(STRING "(.*)"\)/, G) {
	gsub(/'3/,"\n",G[1]) # strings are encoded using '3 for newline,
	gsub(/'2/,"\"",G[1]) #                       and '2 for " character,
	gsub(/'1/,"'", G[1]) #                       and '1 for '.

	n = split(G[1], chars, "")
	unicodeq = !ansiq(chars,n)

	printf "%d ", unicodeq ? 1 : (n == 0 ? 1 : 2)     # tag = obj/bytes
	printf "%d ", unicodeq ? 5 : 3       # type-string/type-string-wide
	printf "%s ", number(n)              # size

	if (unicodeq) {
		for (i = 1; i <= n; i++) {
			printf "%d ", 0             # not a reference
			printf "%d ", 0             # type-enum+
			printf "%s ", number(ord[chars[i]]) # rune itself
		}
	}
	else {
		for (i = 1; i <= n; i++)        # string itself
			printf "%d ", ord[chars[i]]
	}
	print "" #done
}

match($0, /^\(SYMBOL \(\((.+)\)\)\)/, G) {
	n = split(G[1], N, " ")

	printf "%d ", 1      # tag = object
	printf "%d ", 4      # type-symbol
	printf "%d ", 1      # size = 1
	printf "%s ", number(-N[2]) # reference to the symbol name
	print "" #done
}

match($0, /^\(INEXACT \((.+)\)\)/, G) {
	printf "%d ", 2      # tag = bytestream
	printf "%d ", 44     # type-inexact
	printf "%d ", split(G[1], _, " ") # size (typically 8)
	printf "%s ", G[1]   # inexact bytes itself
	print "" #done
}

match($0, /^\(BYTECODE \(\((.+)\)\)\)/, G) {
	gsub(/[()]/,"",G[1])

	gsub(/RET/, "24", G[1])
	gsub(/GOTO/, "2", G[1])
	gsub(/CLOS/, "3", G[1])

	gsub(/REFI/, "1", G[1])
	gsub(/MOVE/, "9", G[1])
	gsub(/MOV2/, "5", G[1])

	gsub(/CAST/, "22", G[1])
	gsub(/SET!/, "43", G[1])

	gsub(/JEQ/,  "8", G[1])
	gsub(/JAF/, "11", G[1])
	gsub(/JAX/, "12", G[1])

	gsub(/JZ/,  "16", G[1])
	gsub(/JN/,  "80", G[1])
	gsub(/JE/, "144", G[1])
	gsub(/JF/, "208", G[1])

	gsub(/EQ\?/, "54", G[1])
	gsub(/LESS\?/, "44", G[1])

	gsub(/SETREF!/, "74", G[1])
	gsub(/SETREF/, "10", G[1])

	gsub(/LDI/,  "13", G[1])
	gsub(/LDN/,  "77", G[1])
	gsub(/LDT/, "141", G[1])
	gsub(/LDF/, "205", G[1])
	gsub(/LD/, "14", G[1])

	gsub(/\yCAR\y/, "52", G[1])
	gsub(/\yCDR\y/, "53", G[1])
	gsub(/\yREF\y/, "47", G[1])

	gsub(/\yCONS\y/, "51", G[1])
	gsub(/\yTYPE\y/, "15", G[1])
	gsub(/\ySIZE\y/, "36", G[1])

	gsub(/ADD/, "38", G[1])
	gsub(/SUB/, "40", G[1])
	gsub(/DIV/, "26", G[1])
	gsub(/MUL/, "39", G[1])

	gsub(/AND/, "55", G[1])
	gsub(/IOR/, "56", G[1])
	gsub(/XOR/, "57", G[1])
	gsub(/SHR/, "58", G[1])
	gsub(/SHL/, "59", G[1])
	gsub(/FP1/, "33", G[1])
	gsub(/FP2/, "34", G[1])

	gsub(/FF-APPLY/,    "49", G[1])
	gsub(/FF:BLACK/,    "42", G[1])
	gsub(/FF:RED\?/,    "41", G[1])
	gsub(/FF:RED/,     "106", G[1])
	gsub(/FF:TOGGLE/,   "46", G[1])
	gsub(/FF:RIGHT\?/, "105", G[1])

	gsub(/CLOCK/, "61", G[1])
	gsub(/SYSCALL/, "63", G[1])

	gsub(/NEW/, "23", G[1])
	gsub(/MAKE/, "18", G[1])
	gsub(/ALLOC/, "82", G[1])

	gsub(/MCP/, "27", G[1])
	gsub(/RUN/, "50", G[1])

	gsub(/\yVERSION\y/, "28", G[1])
	gsub(/\yFEATURES\y/, "29", G[1])
	gsub(/\yVMAX\y/, "30", G[1])
	gsub(/\yVSIZE\y/, "31", G[1])

	gsub(/\yPIN\y/, "35", G[1])
	gsub(/\yUNPIN\y/, "60", G[1])
	gsub(/\yDEREF\y/, "25", G[1])

	gsub(/ARITY-ERROR/, "17", G[1])
	gsub(/APPLY\/CC/, "84", G[1])
	gsub(/VECTOR-APPLY/, "32", G[1])
	gsub(/APPLY/, "20", G[1])

	gsub(/EXIT/, "37", G[1])

	printf "%d ", 2      # tag = bytestream
	printf "%d ", 16     # type-bytecode
	printf "%s ", number(split(G[1], _, " ")) # size
	printf "%s ", G[1]
	print "" #done
}

# match($0, /^\((PAIR|PROCEDURE|CLOSURE|RATIONAL|VECTOR|RED|BLACK-RIGHT|BLACK|CONSTRUCTOR) \((.*)\)\)/, G) {
# 	S = G[2]
# 	print G[0]
# }

match($0, /^\((PAIR|PROCEDURE|CLOSURE|RATIONAL|VECTOR|RED|BLACK-RIGHT|BLACK|CONSTRUCTOR) \((.*)\)\)/, G) {
	S = G[2]

	gsub(/\yNULL\y/,  "(" NULL ")", S)
	gsub(/\yEMPTY\y/, "(" EMPTY ")", S)
	gsub(/\yEOF\y/,   "(" EOF ")", S)

	gsub(/\yTRUE\y/,  "(" TRUE ")", S)
	gsub(/\yFALSE\y/, "(" FALSE ")", S)

	gsub(/\ySTDIN\y/, "(" STDIN  ")", S)
	gsub(/\ySTDOUT\y/,"(" STDOUT ")", S)
	gsub(/\ySTDERR\y/,"(" STDERR ")", S)

	gsub(/^\(|\)$/, "", S)
	n = split(S, N, "\\) \\(")

	printf "%d ", 1      # tag = object
	printf "%d ",        # type
		G[1] ~ "PAIR" ? 1       : # type-pair
		G[1] ~ "PROCEDURE" ? 17 : # type-procedure
		G[1] ~ "CLOSURE" ? 18   : # type-closure
		G[1] ~ "RATIONAL" ? 42  : # type-rational
		G[1] ~ "VECTOR" ? 2     : # type-vector
		# ff types
		G[1] ~ "RED" ? 26 :
		G[1] ~ "BLACK-RIGHT" ? 25 :
		G[1] ~ "BLACK" ? 24 :
		G[1] ~ "CONSTRUCTOR" ? 63 :
		"ERROR" # unrecognized token
	printf "%s ", number(n)  # size

	for (i = 1; i <= n; i++) {
		if (N[i] ~ /^N -/) {
			split(N[i], s, " -")
			printf "%s ", number(s[2])
			continue;
		}
		if (N[i] ~ /^INTEGER /) {
			split(N[i], s, " ")
			printf "%d ", 0  # not a reference
			printf "%d ", s[2] < 0 ? 32 : 0  # type: positive 0, negative 32
			printf "%s ", number(s[2])
			continue;
		}
		# else
		printf "%s ", N[i]
	}
	print "" # done
}
