#; Lu + Ll + Lt + Lm + Lo + Nl + Other_Alphabetic
# Lu - Letter, Uppercase
# Ll - Letter, Lowercase
# Lt - Letter, Titlecase
# Lm - Letter, Modifier
# Lo - Letter, Other
# Nl - Number, Letter
# Other_Alphabetic

libraries/scheme/unicode/alphabetic-chars.scm:
	echo "(define alphabetic-chars-alist '(" | tee $@ >/dev/null
	curl https://www.unicode.org/Public/14.0.0/ucd/UnicodeData.txt \
	    |cut -d';' -f1,3 \
	    |sed -n '/;\(Lu\|Ll\|Lt\|Lm\|Lo\|Nl\)/p' \
	    |cut -d';' -f1 \
	    |sed 's/^\(.*\)$$/(#x\1 #T)/' \
	    |tee -a $@ >/dev/null
	echo '; Other_Alphabetic' \
	    | tee -a $@ >/dev/null
	curl https://www.unicode.org/Public/14.0.0/ucd/PropList.txt \
	    |sed -n '/Other_Alphabetic/p' \
	    |cut -d' ' -f1   \
	    |cut -d'_' -f1,3 \
	    |sed 's/\.\./_/' \
	    |sed 's/^/_/' \
	    |ol tools/translate_unicode_codes.scm \
	    |tee -a $@ >/dev/null
	echo '))' |tee -a $@ >/dev/null
