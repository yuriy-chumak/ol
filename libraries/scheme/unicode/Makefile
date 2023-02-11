# http://www.unicode.org/reports/tr44/#Other_Alphabetic
UNICODE_SERVER=https://www.unicode.org/Public
UNICODE_VERSION=14.0.0

$(shell mkdir -p tmp)
tmp/PropList.txt:
	curl $(UNICODE_SERVER)/$(UNICODE_VERSION)/ucd/PropList.txt -o $@
tmp/UnicodeData.txt:
	curl $(UNICODE_SERVER)/$(UNICODE_VERSION)/ucd/UnicodeData.txt -o $@

unicode: numeric-chars.scm \
         alphabetic-chars.scm \
         whitespace-chars.scm

# Nd + Nl + No:
# Nd - Number, Decimal Digit
# Nl - Number, Letter
# No - Number, Other
numeric-chars.scm: tmp/UnicodeData.txt
	echo "(define numeric-chars-alist '(" | tee $@ >/dev/null
	echo '; Nd + Nl + No' \
	    |tee -a $@ >/dev/null
	cat tmp/UnicodeData.txt \
	    |cut -d';' -f1,2,3 \
	    |sed -n '/;\(Nd\|Nl\|No\)/p' \
	    |sed '/DIGIT ZERO/s/^\([^;]*\).*$$/(#x\1 . 0)/' \
	    |sed '/DIGIT ONE/s/^\([^;]*\).*$$/(#x\1 . 1)/' \
	    |sed '/DIGIT TWO/s/^\([^;]*\).*$$/(#x\1 . 2)/' \
	    |sed '/DIGIT THREE/s/^\([^;]*\).*$$/(#x\1 . 3)/' \
	    |sed '/DIGIT FOUR/s/^\([^;]*\).*$$/(#x\1 . 4)/' \
	    |sed '/DIGIT FIVE/s/^\([^;]*\).*$$/(#x\1 . 5)/' \
	    |sed '/DIGIT SIX/s/^\([^;]*\).*$$/(#x\1 . 6)/' \
	    |sed '/DIGIT SEVEN/s/^\([^;]*\).*$$/(#x\1 . 7)/' \
	    |sed '/DIGIT EIGHT/s/^\([^;]*\).*$$/(#x\1 . 8)/' \
	    |sed '/DIGIT NINE/s/^\([^;]*\).*$$/(#x\1 . 9)/' \
	    |sed '/;/s/^\([^;]*\).*$$/(#x\1 . #T)/' \
	    |tee -a $@ >/dev/null
	echo '))' |tee -a $@ >/dev/null

# Lu + Ll + Lt + Lm + Lo + Nl + Other_Alphabetic:
# Lu - Letter, Uppercase
# Ll - Letter, Lowercase
# Lt - Letter, Titlecase
# Lm - Letter, Modifier
# Lo - Letter, Other
# Nl - Number, Letter
# Other_Alphabetic, Used in deriving the Alphabetic property.
alphabetic-chars.scm: tmp/UnicodeData.txt tmp/PropList.txt
	echo "(define alphabetic-chars-alist '(" | tee $@ >/dev/null
	echo '; Lu + Ll + Lt + Lm + Lo + Nl + Other_Alphabetic' \
	    | tee -a $@ >/dev/null
	cat tmp/UnicodeData.txt \
	    |cut -d';' -f1,3 \
	    |sed -n '/;\(Lu\|Ll\|Lt\|Lm\|Lo\|Nl\)/p' \
	    |cut -d';' -f1 \
	    |sed 's/^\(.*\)$$/#x\1 /' \
	    |tee -a $@ >/dev/null
	cat tmp/PropList.txt \
	    |sed -n '/; Other_Alphabetic/p' \
	    |cut -d' ' -f1   \
	    |cut -d'_' -f1,3 \
	    |sed 's/\.\./_/' \
	    |sed 's/^/_/' \
	    |awk -f ../../../tools/translate_unicode_codes.awk \
	    |tee -a $@ >/dev/null
	echo '))' |tee -a $@ >/dev/null

# White_Space, spaces
whitespace-chars.scm: tmp/PropList.txt
	echo "(define whitespace-chars-alist '(" | tee $@ >/dev/null
	echo '; White_Space' \
	    | tee -a $@ >/dev/null
	cat tmp/PropList.txt \
	    |sed -n '/; White_Space/p' \
	    |cut -d' ' -f1   \
	    |cut -d'_' -f1,3 \
	    |sed 's/\.\./_/' \
	    |sed 's/^/_/' \
	    |awk -f ../../../tools/translate_unicode_codes.awk \
	    |tee -a $@ >/dev/null
	echo '))' |tee -a $@ >/dev/null
